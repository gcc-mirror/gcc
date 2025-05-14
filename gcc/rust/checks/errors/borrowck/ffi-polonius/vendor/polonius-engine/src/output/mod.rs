// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use datafrog::Relation;
use rustc_hash::{FxHashMap, FxHashSet};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use crate::facts::{AllFacts, Atom, FactTypes};

mod datafrog_opt;
mod initialization;
mod liveness;
mod location_insensitive;
mod naive;

#[derive(Debug, Clone, Copy)]
pub enum Algorithm {
    /// Simple rules, but slower to execute
    Naive,

    /// Optimized variant of the rules
    DatafrogOpt,

    /// Fast to compute, but imprecise: there can be false-positives
    /// but no false-negatives. Tailored for quick "early return" situations.
    LocationInsensitive,

    /// Compares the `Naive` and `DatafrogOpt` variants to ensure they indeed
    /// compute the same errors.
    Compare,

    /// Combination of the fast `LocationInsensitive` pre-pass, followed by
    /// the more expensive `DatafrogOpt` variant.
    Hybrid,
}

impl Algorithm {
    /// Optimized variants that ought to be equivalent to "naive"
    pub const OPTIMIZED: &'static [Algorithm] = &[Algorithm::DatafrogOpt];

    pub fn variants() -> [&'static str; 5] {
        [
            "Naive",
            "DatafrogOpt",
            "LocationInsensitive",
            "Compare",
            "Hybrid",
        ]
    }
}

impl ::std::str::FromStr for Algorithm {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_ref() {
            "naive" => Ok(Algorithm::Naive),
            "datafrogopt" => Ok(Algorithm::DatafrogOpt),
            "locationinsensitive" => Ok(Algorithm::LocationInsensitive),
            "compare" => Ok(Algorithm::Compare),
            "hybrid" => Ok(Algorithm::Hybrid),
            _ => Err(String::from(
                "valid values: Naive, DatafrogOpt, LocationInsensitive, Compare, Hybrid",
            )),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Output<T: FactTypes> {
    pub errors: FxHashMap<T::Point, Vec<T::Loan>>,
    pub subset_errors: FxHashMap<T::Point, BTreeSet<(T::Origin, T::Origin)>>,
    pub move_errors: FxHashMap<T::Point, Vec<T::Path>>,

    pub dump_enabled: bool,

    // these are just for debugging
    pub loan_live_at: FxHashMap<T::Point, Vec<T::Loan>>,
    pub origin_contains_loan_at: FxHashMap<T::Point, BTreeMap<T::Origin, BTreeSet<T::Loan>>>,
    pub origin_contains_loan_anywhere: FxHashMap<T::Origin, BTreeSet<T::Loan>>,
    pub origin_live_on_entry: FxHashMap<T::Point, Vec<T::Origin>>,
    pub loan_invalidated_at: FxHashMap<T::Point, Vec<T::Loan>>,
    pub subset: FxHashMap<T::Point, BTreeMap<T::Origin, BTreeSet<T::Origin>>>,
    pub subset_anywhere: FxHashMap<T::Origin, BTreeSet<T::Origin>>,
    pub var_live_on_entry: FxHashMap<T::Point, Vec<T::Variable>>,
    pub var_drop_live_on_entry: FxHashMap<T::Point, Vec<T::Variable>>,
    pub path_maybe_initialized_on_exit: FxHashMap<T::Point, Vec<T::Path>>,
    pub path_maybe_uninitialized_on_exit: FxHashMap<T::Point, Vec<T::Path>>,
    pub known_contains: FxHashMap<T::Origin, BTreeSet<T::Loan>>,
    pub var_maybe_partly_initialized_on_exit: FxHashMap<T::Point, Vec<T::Variable>>,
}

/// Subset of `AllFacts` dedicated to initialization
struct InitializationContext<T: FactTypes> {
    child_path: Vec<(T::Path, T::Path)>,
    path_is_var: Vec<(T::Path, T::Variable)>,
    path_assigned_at_base: Vec<(T::Path, T::Point)>,
    path_moved_at_base: Vec<(T::Path, T::Point)>,
    path_accessed_at_base: Vec<(T::Path, T::Point)>,
}

/// Subset of `AllFacts` dedicated to liveness
struct LivenessContext<T: FactTypes> {
    var_used_at: Vec<(T::Variable, T::Point)>,
    var_defined_at: Vec<(T::Variable, T::Point)>,
    var_dropped_at: Vec<(T::Variable, T::Point)>,
    use_of_var_derefs_origin: Vec<(T::Variable, T::Origin)>,
    drop_of_var_derefs_origin: Vec<(T::Variable, T::Origin)>,
}

/// Subset of `AllFacts` dedicated to borrow checking, and data ready to use by the variants
struct Context<'ctx, T: FactTypes> {
    // `Relation`s used as static inputs, by all variants
    origin_live_on_entry: Relation<(T::Origin, T::Point)>,
    loan_invalidated_at: Relation<(T::Loan, T::Point)>,

    // static inputs used via `Variable`s, by all variants
    subset_base: &'ctx Vec<(T::Origin, T::Origin, T::Point)>,
    loan_issued_at: &'ctx Vec<(T::Origin, T::Loan, T::Point)>,

    // static inputs used by variants other than `LocationInsensitive`
    loan_killed_at: Relation<(T::Loan, T::Point)>,
    known_contains: Relation<(T::Origin, T::Loan)>,
    placeholder_origin: Relation<(T::Origin, ())>,
    placeholder_loan: Relation<(T::Loan, T::Origin)>,

    // The `known_placeholder_subset` relation in the facts does not necessarily contain all the
    // transitive subsets. The transitive closure is always needed, so this version here is fully
    // closed over.
    known_placeholder_subset: Relation<(T::Origin, T::Origin)>,

    // while this static input is unused by `LocationInsensitive`, it's depended on by
    // initialization and liveness, so already computed by the time we get to borrowcking.
    cfg_edge: Relation<(T::Point, T::Point)>,

    // Partial results possibly used by other variants as input. Not currently used yet.
    #[allow(dead_code)]
    potential_errors: Option<FxHashSet<T::Loan>>,
    #[allow(dead_code)]
    potential_subset_errors: Option<Relation<(T::Origin, T::Origin)>>,
}

impl<T: FactTypes> Output<T> {
    /// All variants require the same initial preparations, done in multiple
    /// successive steps:
    /// - compute initialization data
    /// - compute liveness
    /// - prepare static inputs as shared `Relation`s
    /// - in cases where `LocationInsensitive` variant is ran as a filtering pre-pass,
    ///   partial results can also be stored in the context, so that the following
    ///   variant can use it to prune its own input data
    pub fn compute(all_facts: &AllFacts<T>, algorithm: Algorithm, dump_enabled: bool) -> Self {
        let mut result = Output::new(dump_enabled);

        // TODO: remove all the cloning thereafter, but that needs to be done in concert with rustc

        let cfg_edge = all_facts.cfg_edge.clone().into();

        // 1) Initialization
        let initialization_ctx = InitializationContext {
            child_path: all_facts.child_path.clone(),
            path_is_var: all_facts.path_is_var.clone(),
            path_assigned_at_base: all_facts.path_assigned_at_base.clone(),
            path_moved_at_base: all_facts.path_moved_at_base.clone(),
            path_accessed_at_base: all_facts.path_accessed_at_base.clone(),
        };

        let initialization::InitializationResult::<T>(
            var_maybe_partly_initialized_on_exit,
            move_errors,
        ) = initialization::compute(initialization_ctx, &cfg_edge, &mut result);

        // FIXME: move errors should prevent the computation from continuing: we can't compute
        // liveness and analyze loans accurately when there are move errors, and should early
        // return here.
        for &(path, location) in move_errors.iter() {
            result.move_errors.entry(location).or_default().push(path);
        }

        // 2) Liveness
        let liveness_ctx = LivenessContext {
            var_used_at: all_facts.var_used_at.clone(),
            var_defined_at: all_facts.var_defined_at.clone(),
            var_dropped_at: all_facts.var_dropped_at.clone(),
            use_of_var_derefs_origin: all_facts.use_of_var_derefs_origin.clone(),
            drop_of_var_derefs_origin: all_facts.drop_of_var_derefs_origin.clone(),
        };

        let mut origin_live_on_entry = liveness::compute_live_origins(
            liveness_ctx,
            &cfg_edge,
            var_maybe_partly_initialized_on_exit,
            &mut result,
        );

        let cfg_node = cfg_edge
            .iter()
            .map(|&(point1, _)| point1)
            .chain(cfg_edge.iter().map(|&(_, point2)| point2))
            .collect();

        liveness::make_universal_regions_live::<T>(
            &mut origin_live_on_entry,
            &cfg_node,
            &all_facts.universal_region,
        );

        // 3) Borrow checking

        // Prepare data as datafrog relations, ready to join.
        //
        // Note: if rustc and polonius had more interaction, we could also delay or avoid
        // generating some of the facts that are now always present here. For example,
        // the `LocationInsensitive` variant doesn't use the `loan_killed_at` relation, so we could
        // technically delay computing and passing it from rustc, when using this or the `Hybrid`
        // variants, to after the pre-pass has made sure we actually need to compute the full
        // analysis. If these facts happened to be recorded in separate MIR walks, we might also
        // avoid generating those facts.

        let origin_live_on_entry = origin_live_on_entry.into();

        // TODO: also flip the order of this relation's arguments in rustc
        // from `loan_invalidated_at(point, loan)` to `loan_invalidated_at(loan, point)`.
        // to avoid this allocation.
        let loan_invalidated_at = Relation::from_iter(
            all_facts
                .loan_invalidated_at
                .iter()
                .map(|&(point, loan)| (loan, point)),
        );

        let loan_killed_at = all_facts.loan_killed_at.clone().into();

        // `known_placeholder_subset` is a list of all the `'a: 'b` subset relations the user gave:
        // it's not required to be transitive. `known_contains` is its transitive closure: a list
        // of all the known placeholder loans that each of these placeholder origins contains.
        // Given the `known_placeholder_subset`s `'a: 'b` and `'b: 'c`: in the `known_contains`
        // relation, `'a` will also contain `'c`'s placeholder loan.
        let known_placeholder_subset = all_facts.known_placeholder_subset.clone().into();
        let known_contains =
            Output::<T>::compute_known_contains(&known_placeholder_subset, &all_facts.placeholder);

        // Fully close over the `known_placeholder_subset` relation.
        let known_placeholder_subset =
            Output::<T>::compute_known_placeholder_subset(&known_placeholder_subset);

        let placeholder_origin: Relation<_> = Relation::from_iter(
            all_facts
                .universal_region
                .iter()
                .map(|&origin| (origin, ())),
        );

        let placeholder_loan = Relation::from_iter(
            all_facts
                .placeholder
                .iter()
                .map(|&(origin, loan)| (loan, origin)),
        );

        // Ask the variants to compute errors in their own way
        let mut ctx = Context {
            origin_live_on_entry,
            loan_invalidated_at,
            cfg_edge,
            subset_base: &all_facts.subset_base,
            loan_issued_at: &all_facts.loan_issued_at,
            loan_killed_at,
            known_contains,
            known_placeholder_subset,
            placeholder_origin,
            placeholder_loan,
            potential_errors: None,
            potential_subset_errors: None,
        };

        let (errors, subset_errors) = match algorithm {
            Algorithm::LocationInsensitive => {
                let (potential_errors, potential_subset_errors) =
                    location_insensitive::compute(&ctx, &mut result);

                // Note: the error location is meaningless for a location-insensitive
                // subset error analysis. This is acceptable here as this variant is not one
                // which should be used directly besides debugging, the `Hybrid` variant will
                // take advantage of its result.
                let potential_subset_errors: Relation<(T::Origin, T::Origin, T::Point)> =
                    Relation::from_iter(
                        potential_subset_errors
                            .into_iter()
                            .map(|&(origin1, origin2)| (origin1, origin2, 0.into())),
                    );

                (potential_errors, potential_subset_errors)
            }
            Algorithm::Naive => naive::compute(&ctx, &mut result),
            Algorithm::DatafrogOpt => datafrog_opt::compute(&ctx, &mut result),
            Algorithm::Hybrid => {
                // Execute the fast `LocationInsensitive` computation as a pre-pass:
                // if it finds no possible errors, we don't need to do the more complex
                // computations as they won't find errors either, and we can return early.
                let (potential_errors, potential_subset_errors) =
                    location_insensitive::compute(&ctx, &mut result);

                if potential_errors.is_empty() && potential_subset_errors.is_empty() {
                    // There are no loan errors, nor subset errors, we can early return
                    // empty errors lists and avoid doing the heavy analysis.
                    (potential_errors, Vec::new().into())
                } else {
                    // Record these potential errors as they can be used to limit the next
                    // variant's work to only these loans.
                    ctx.potential_errors =
                        Some(potential_errors.iter().map(|&(loan, _)| loan).collect());
                    ctx.potential_subset_errors = Some(potential_subset_errors);

                    datafrog_opt::compute(&ctx, &mut result)
                }
            }
            Algorithm::Compare => {
                // Ensure the `Naive` and `DatafrogOpt` errors are the same
                let (naive_errors, naive_subset_errors) = naive::compute(&ctx, &mut result);
                let (opt_errors, _) = datafrog_opt::compute(&ctx, &mut result);

                // TODO: compare illegal subset relations errors as well here ?

                let mut naive_errors_by_point = FxHashMap::default();
                for &(loan, point) in naive_errors.iter() {
                    naive_errors_by_point
                        .entry(point)
                        .or_insert_with(Vec::new)
                        .push(loan);
                }

                let mut opt_errors_by_point = FxHashMap::default();
                for &(loan, point) in opt_errors.iter() {
                    opt_errors_by_point
                        .entry(point)
                        .or_insert_with(Vec::new)
                        .push(loan);
                }

                if compare_errors(&naive_errors_by_point, &opt_errors_by_point) {
                    panic!(concat!(
                        "The errors reported by the naive algorithm differ from ",
                        "the errors reported by the optimized algorithm. ",
                        "See the error log for details."
                    ));
                } else {
                    debug!("Naive and optimized algorithms reported the same errors.");
                }

                (naive_errors, naive_subset_errors)
            }
        };

        // Record illegal access errors
        for &(loan, location) in errors.iter() {
            result.errors.entry(location).or_default().push(loan);
        }

        // Record illegal subset errors
        for &(origin1, origin2, location) in subset_errors.iter() {
            result
                .subset_errors
                .entry(location)
                .or_default()
                .insert((origin1, origin2));
        }

        // Record more debugging info when asked to do so
        if dump_enabled {
            for &(origin, location) in ctx.origin_live_on_entry.iter() {
                result
                    .origin_live_on_entry
                    .entry(location)
                    .or_default()
                    .push(origin);
            }

            for &(origin, loan) in ctx.known_contains.iter() {
                result
                    .known_contains
                    .entry(origin)
                    .or_default()
                    .insert(loan);
            }
        }

        result
    }

    /// Computes the transitive closure of the `known_placeholder_subset` relation, so that we have
    /// the full list of placeholder loans contained by the placeholder origins.
    fn compute_known_contains(
        known_placeholder_subset: &Relation<(T::Origin, T::Origin)>,
        placeholder: &[(T::Origin, T::Loan)],
    ) -> Relation<(T::Origin, T::Loan)> {
        let mut iteration = datafrog::Iteration::new();
        let known_contains = iteration.variable("known_contains");

        // known_contains(Origin1, Loan1) :-
        //   placeholder(Origin1, Loan1).
        known_contains.extend(placeholder.iter());

        while iteration.changed() {
            // known_contains(Origin2, Loan1) :-
            //   known_contains(Origin1, Loan1),
            //   known_placeholder_subset(Origin1, Origin2).
            known_contains.from_join(
                &known_contains,
                known_placeholder_subset,
                |&_origin1, &loan1, &origin2| (origin2, loan1),
            );
        }

        known_contains.complete()
    }

    /// Computes the transitive closure of the `known_placeholder_subset` relation.
    fn compute_known_placeholder_subset(
        known_placeholder_subset_base: &Relation<(T::Origin, T::Origin)>,
    ) -> Relation<(T::Origin, T::Origin)> {
        use datafrog::{Iteration, RelationLeaper};
        let mut iteration = Iteration::new();

        let known_placeholder_subset = iteration.variable("known_placeholder_subset");

        // known_placeholder_subset(Origin1, Origin2) :-
        //   known_placeholder_subset_base(Origin1, Origin2).
        known_placeholder_subset.extend(known_placeholder_subset_base.iter());

        while iteration.changed() {
            // known_placeholder_subset(Origin1, Origin3) :-
            //   known_placeholder_subset(Origin1, Origin2),
            //   known_placeholder_subset_base(Origin2, Origin3).
            known_placeholder_subset.from_leapjoin(
                &known_placeholder_subset,
                known_placeholder_subset_base.extend_with(|&(_origin1, origin2)| origin2),
                |&(origin1, _origin2), &origin3| (origin1, origin3),
            );
        }

        known_placeholder_subset.complete()
    }

    fn new(dump_enabled: bool) -> Self {
        Output {
            errors: FxHashMap::default(),
            subset_errors: FxHashMap::default(),
            dump_enabled,
            loan_live_at: FxHashMap::default(),
            origin_contains_loan_at: FxHashMap::default(),
            origin_contains_loan_anywhere: FxHashMap::default(),
            origin_live_on_entry: FxHashMap::default(),
            loan_invalidated_at: FxHashMap::default(),
            move_errors: FxHashMap::default(),
            subset: FxHashMap::default(),
            subset_anywhere: FxHashMap::default(),
            var_live_on_entry: FxHashMap::default(),
            var_drop_live_on_entry: FxHashMap::default(),
            path_maybe_initialized_on_exit: FxHashMap::default(),
            path_maybe_uninitialized_on_exit: FxHashMap::default(),
            var_maybe_partly_initialized_on_exit: FxHashMap::default(),
            known_contains: FxHashMap::default(),
        }
    }

    pub fn errors_at(&self, location: T::Point) -> &[T::Loan] {
        match self.errors.get(&location) {
            Some(v) => v,
            None => &[],
        }
    }

    pub fn loans_in_scope_at(&self, location: T::Point) -> &[T::Loan] {
        match self.loan_live_at.get(&location) {
            Some(p) => p,
            None => &[],
        }
    }

    pub fn origin_contains_loan_at(
        &self,
        location: T::Point,
    ) -> Cow<'_, BTreeMap<T::Origin, BTreeSet<T::Loan>>> {
        assert!(self.dump_enabled);
        match self.origin_contains_loan_at.get(&location) {
            Some(map) => Cow::Borrowed(map),
            None => Cow::Owned(BTreeMap::default()),
        }
    }

    pub fn origins_live_at(&self, location: T::Point) -> &[T::Origin] {
        assert!(self.dump_enabled);
        match self.origin_live_on_entry.get(&location) {
            Some(v) => v,
            None => &[],
        }
    }

    pub fn subsets_at(
        &self,
        location: T::Point,
    ) -> Cow<'_, BTreeMap<T::Origin, BTreeSet<T::Origin>>> {
        assert!(self.dump_enabled);
        match self.subset.get(&location) {
            Some(v) => Cow::Borrowed(v),
            None => Cow::Owned(BTreeMap::default()),
        }
    }
}

/// Compares errors reported by Naive implementation with the errors
/// reported by the optimized implementation.
fn compare_errors<Loan: Atom, Point: Atom>(
    all_naive_errors: &FxHashMap<Point, Vec<Loan>>,
    all_opt_errors: &FxHashMap<Point, Vec<Loan>>,
) -> bool {
    let points = all_naive_errors.keys().chain(all_opt_errors.keys());

    let mut differ = false;
    for point in points {
        let mut naive_errors = all_naive_errors.get(&point).cloned().unwrap_or_default();
        naive_errors.sort();

        let mut opt_errors = all_opt_errors.get(&point).cloned().unwrap_or_default();
        opt_errors.sort();

        for err in naive_errors.iter() {
            if !opt_errors.contains(err) {
                error!(
                    "Error {0:?} at {1:?} reported by naive, but not opt.",
                    err, point
                );
                differ = true;
            }
        }

        for err in opt_errors.iter() {
            if !naive_errors.contains(err) {
                error!(
                    "Error {0:?} at {1:?} reported by opt, but not naive.",
                    err, point
                );
                differ = true;
            }
        }
    }

    differ
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Atom for usize {
        fn index(self) -> usize {
            self
        }
    }

    fn compare(
        errors1: &FxHashMap<usize, Vec<usize>>,
        errors2: &FxHashMap<usize, Vec<usize>>,
    ) -> bool {
        let diff1 = compare_errors(errors1, errors2);
        let diff2 = compare_errors(errors2, errors1);
        assert_eq!(diff1, diff2);
        diff1
    }

    #[test]
    fn test_compare_errors() {
        let empty = FxHashMap::default();
        assert_eq!(false, compare(&empty, &empty));
        let mut empty_vec = FxHashMap::default();
        empty_vec.insert(1, vec![]);
        empty_vec.insert(2, vec![]);
        assert_eq!(false, compare(&empty, &empty_vec));

        let mut singleton1 = FxHashMap::default();
        singleton1.insert(1, vec![10]);
        assert_eq!(false, compare(&singleton1, &singleton1));
        let mut singleton2 = FxHashMap::default();
        singleton2.insert(1, vec![11]);
        assert_eq!(false, compare(&singleton2, &singleton2));
        let mut singleton3 = FxHashMap::default();
        singleton3.insert(2, vec![10]);
        assert_eq!(false, compare(&singleton3, &singleton3));

        assert_eq!(true, compare(&singleton1, &singleton2));
        assert_eq!(true, compare(&singleton2, &singleton3));
        assert_eq!(true, compare(&singleton1, &singleton3));

        assert_eq!(true, compare(&empty, &singleton1));
        assert_eq!(true, compare(&empty, &singleton2));
        assert_eq!(true, compare(&empty, &singleton3));

        let mut errors1 = FxHashMap::default();
        errors1.insert(1, vec![11]);
        errors1.insert(2, vec![10]);
        assert_eq!(false, compare(&errors1, &errors1));
        assert_eq!(true, compare(&errors1, &singleton1));
        assert_eq!(true, compare(&errors1, &singleton2));
        assert_eq!(true, compare(&errors1, &singleton3));
    }
}
