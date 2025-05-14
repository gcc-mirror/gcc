// Copyright 2019 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! An implementation of the origin liveness calculation logic

use std::collections::BTreeSet;
use std::time::Instant;

use crate::facts::FactTypes;
use crate::output::{LivenessContext, Output};

use datafrog::{Iteration, Relation, RelationLeaper};

pub(super) fn compute_live_origins<T: FactTypes>(
    ctx: LivenessContext<T>,
    cfg_edge: &Relation<(T::Point, T::Point)>,
    var_maybe_partly_initialized_on_exit: Relation<(T::Variable, T::Point)>,
    output: &mut Output<T>,
) -> Vec<(T::Origin, T::Point)> {
    let timer = Instant::now();
    let mut iteration = Iteration::new();

    // Relations
    let var_defined_at: Relation<(T::Variable, T::Point)> = ctx.var_defined_at.into();
    let cfg_edge_reverse: Relation<(T::Point, T::Point)> = cfg_edge
        .iter()
        .map(|&(point1, point2)| (point2, point1))
        .collect();
    let use_of_var_derefs_origin: Relation<(T::Variable, T::Origin)> =
        ctx.use_of_var_derefs_origin.into();
    let drop_of_var_derefs_origin: Relation<(T::Variable, T::Origin)> =
        ctx.drop_of_var_derefs_origin.into();
    let var_dropped_at: Relation<((T::Variable, T::Point), ())> = ctx
        .var_dropped_at
        .into_iter()
        .map(|(var, point)| ((var, point), ()))
        .collect();

    // Variables

    // `var_live_on_entry`: variable `var` is live upon entry at `point`
    let var_live_on_entry = iteration.variable::<(T::Variable, T::Point)>("var_live_on_entry");
    // `var_drop_live_on_entry`: variable `var` is drop-live (will be used for a drop) upon entry in `point`
    let var_drop_live_on_entry =
        iteration.variable::<(T::Variable, T::Point)>("var_drop_live_on_entry");

    // This is what we are actually calculating:
    let origin_live_on_entry = iteration.variable::<(T::Origin, T::Point)>("origin_live_on_entry");

    // This propagates the relation `var_live_on_entry(var, point) :- var_used_at(var, point)`:
    var_live_on_entry.insert(ctx.var_used_at.into());

    // var_maybe_partly_initialized_on_entry(var, point2) :-
    //     var_maybe_partly_initialized_on_exit(var, point1),
    //     cfg_edge(point1, point2).
    let var_maybe_partly_initialized_on_entry = Relation::from_leapjoin(
        &var_maybe_partly_initialized_on_exit,
        cfg_edge.extend_with(|&(_var, point1)| point1),
        |&(var, _point1), &point2| ((var, point2), ()),
    );

    // var_drop_live_on_entry(var, point) :-
    //     var_dropped_at(var, point),
    //     var_maybe_partly_initialized_on_entry(var, point).
    var_drop_live_on_entry.insert(Relation::from_join(
        &var_dropped_at,
        &var_maybe_partly_initialized_on_entry,
        |&(var, point), _, _| (var, point),
    ));

    while iteration.changed() {
        // origin_live_on_entry(origin, point) :-
        //   var_drop_live_on_entry(var, point),
        //   drop_of_var_derefs_origin(var, origin).
        origin_live_on_entry.from_join(
            &var_drop_live_on_entry,
            &drop_of_var_derefs_origin,
            |_var, &point, &origin| (origin, point),
        );

        // origin_live_on_entry(origin, point) :-
        //   var_live_on_entry(var, point),
        //   use_of_var_derefs_origin(var, origin).
        origin_live_on_entry.from_join(
            &var_live_on_entry,
            &use_of_var_derefs_origin,
            |_var, &point, &origin| (origin, point),
        );

        // var_live_on_entry(var, point1) :-
        //     var_live_on_entry(var, point2),
        //     cfg_edge(point1, point2),
        //     !var_defined(var, point1).
        var_live_on_entry.from_leapjoin(
            &var_live_on_entry,
            (
                var_defined_at.extend_anti(|&(var, _point2)| var),
                cfg_edge_reverse.extend_with(|&(_var, point2)| point2),
            ),
            |&(var, _point2), &point1| (var, point1),
        );

        // var_drop_live_on_entry(Var, SourceNode) :-
        //   var_drop_live_on_entry(Var, TargetNode),
        //   cfg_edge(SourceNode, TargetNode),
        //   !var_defined_at(Var, SourceNode),
        //   var_maybe_partly_initialized_on_exit(Var, SourceNode).
        var_drop_live_on_entry.from_leapjoin(
            &var_drop_live_on_entry,
            (
                var_defined_at.extend_anti(|&(var, _target_node)| var),
                cfg_edge_reverse.extend_with(|&(_var, target_node)| target_node),
                var_maybe_partly_initialized_on_exit.extend_with(|&(var, _target_node)| var),
            ),
            |&(var, _targetnode), &source_node| (var, source_node),
        );
    }

    let origin_live_on_entry = origin_live_on_entry.complete();

    info!(
        "compute_live_origins() completed: {} tuples, {:?}",
        origin_live_on_entry.len(),
        timer.elapsed(),
    );

    if output.dump_enabled {
        let var_drop_live_on_entry = var_drop_live_on_entry.complete();
        for &(var, location) in var_drop_live_on_entry.iter() {
            output
                .var_drop_live_on_entry
                .entry(location)
                .or_default()
                .push(var);
        }

        let var_live_on_entry = var_live_on_entry.complete();
        for &(var, location) in var_live_on_entry.iter() {
            output
                .var_live_on_entry
                .entry(location)
                .or_default()
                .push(var);
        }
    }

    origin_live_on_entry.elements
}

pub(super) fn make_universal_regions_live<T: FactTypes>(
    origin_live_on_entry: &mut Vec<(T::Origin, T::Point)>,
    cfg_node: &BTreeSet<T::Point>,
    universal_regions: &[T::Origin],
) {
    debug!("make_universal_regions_live()");

    origin_live_on_entry.reserve(universal_regions.len() * cfg_node.len());
    for &origin in universal_regions.iter() {
        for &point in cfg_node.iter() {
            origin_live_on_entry.push((origin, point));
        }
    }
}
