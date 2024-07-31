// Copyright (C) 2020-2025 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

mod gccrs_ffi;

use gccrs_ffi::FFIVector;
use polonius_engine::{AllFacts, Atom, FactTypes, Output};
use std::fmt::Debug;
use std::hash::Hash;

extern "C" {
    fn FFIVector__new() -> *mut FFIVector;
    fn FFIVector__new_vec_pair() -> *mut FFIVector;
    fn FFIVector__new_vec_triple() -> *mut FFIVector;
    fn FFIVector__push(vector: *mut FFIVector, element: usize);
    fn FFIVector__push_vec_pair(
        vector: *mut FFIVector,
        element: gccrs_ffi::Pair<usize, *mut FFIVector>,
    );
    fn FFIVector__push_vec_triple(
        vector: *mut FFIVector,
        element: gccrs_ffi::Triple<usize, usize, usize>,
    );
}

/// A single fact value.
/// For simplicity we use one type for all facts.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct GccrsAtom(usize);

impl Atom for GccrsAtom {
    fn index(self) -> usize {
        self.0
    }
}

impl From<usize> for GccrsAtom {
    fn from(inner: usize) -> GccrsAtom {
        GccrsAtom(inner)
    }
}

impl From<GccrsAtom> for usize {
    fn from(atom: GccrsAtom) -> Self {
        atom.index()
    }
}

impl From<&GccrsAtom> for usize {
    fn from(atom: &GccrsAtom) -> Self {
        atom.index()
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct GccrsFacts;

impl FactTypes for GccrsFacts {
    type Origin = GccrsAtom;
    type Loan = GccrsAtom;
    type Point = GccrsAtom;
    type Variable = GccrsAtom;
    type Path = GccrsAtom;
}

impl From<gccrs_ffi::FactsView> for AllFacts<GccrsFacts> {
    fn from(input: gccrs_ffi::FactsView) -> Self {
        AllFacts::<GccrsFacts> {
            loan_issued_at: input.loan_issued_at.into(),
            universal_region: input.universal_region.into(),
            cfg_edge: input.cfg_edge.into(),
            loan_killed_at: input.loan_killed_at.into(),
            subset_base: input.subset_base.into(),
            loan_invalidated_at: input.loan_invalidated_at.into(),
            var_used_at: input.var_used_at.into(),
            var_defined_at: input.var_defined_at.into(),
            var_dropped_at: input.var_dropped_at.into(),
            use_of_var_derefs_origin: input.use_of_var_derefs_origin.into(),
            drop_of_var_derefs_origin: input.drop_of_var_derefs_origin.into(),
            child_path: input.child_path.into(),
            path_is_var: input.path_is_var.into(),
            path_assigned_at_base: input.path_assigned_at_base.into(),
            path_moved_at_base: input.path_moved_at_base.into(),
            path_accessed_at_base: input.path_accessed_at_base.into(),
            known_placeholder_subset: input.known_placeholder_subset.into(),
            placeholder: input.placeholder.into(),
        }
    }
}

fn print_point(point: GccrsAtom) {
    let val: usize = point.into();
    // Point is a 32 bit unsigned integer
    // 16               15              1
    // xxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx x
    // ^~~~~~~~~~~~~~~~ ^~~~~~~~~~~~~~~ ^
    // |                |               |
    // basic_block      |               start/mid
    //                  statement
    // the left most 16 bits store the basic block number
    // the right most bit, represents the start/mid status
    // the remaining 15 bits between these two represent the statement
    let mid = val % 2 == 1;
    let bb = val >> 16;
    // firstly we can get rid of right most bit by performing left shift once
    let hide_left_most_bit = val >> 1;
    // now we only need the 15 bits on the right
    // we can mask the remaining bits by performing bitwise AND with fifteen
    // 1's which in hexadecimal is 0x7FFF
    let stmt = hide_left_most_bit & 0x7FFF;
    eprint!("{}(bb{}[{}])", if mid { "Mid" } else { "Start" }, bb, stmt);
}

/// Run the polonius analysis on the given facts (for a single function).
/// Right now, results are only printed and not propagated back to the gccrs.
#[no_mangle]
pub unsafe extern "C" fn polonius_run(
    input: gccrs_ffi::FactsView,
    dump_enabled: bool,
) -> gccrs_ffi::Output {
    let facts = AllFacts::<GccrsFacts>::from(input);
    let output = Output::compute(
        &facts,
        polonius_engine::Algorithm::DatafrogOpt,
        dump_enabled,
    );

    if dump_enabled {
        eprintln!("Subsets:");
        let mut subset_vec: Vec<_> = output.subset.iter().collect();
        subset_vec.sort_by_key(|&(point, _)| point);
        for (point, subsets) in subset_vec {
            print_point(*point);
            eprintln!(": {{");
            for (&lhs, rhss) in subsets {
                for &rhs in rhss {
                    eprintln!("    {} <= {}", usize::from(lhs), usize::from(rhs));
                }
            }
            eprintln!("}}");
        }

        // Print origin live on entry
        eprintln!("Origin live on entry:");
        let mut origin_vec: Vec<_> = output.origin_live_on_entry.iter().collect();
        origin_vec.sort_by_key(|&(point, _)| point);
        for (point, origins) in origin_vec {
            print_point(*point);
            eprintln!(": {{");
            for &origin in origins {
                eprintln!("    {}", usize::from(origin));
            }
            eprintln!("}}");
        }

        eprintln!("Origin contains loan at:");
        let mut origin_vec: Vec<_> = output.origin_contains_loan_at.iter().collect();
        origin_vec.sort_by_key(|&(point, _)| point);
        for (point, origins) in origin_vec {
            print_point(*point);
            eprintln!(": {{");
            for (&origin, loans) in origins {
                eprintln!(
                    "    {}:{:?}",
                    usize::from(origin),
                    loans.iter().map(|&e| usize::from(e)).collect::<Vec<_>>()
                );
            }
            eprintln!("}}");
        }

        eprintln!("Polonius analysis completed. Results:");
        if output.errors.len() > 0 {
            eprintln!("Errors:");
            for (&point, errors) in &output.errors {
                print_point(point);
                eprintln!(": {:?}", errors);
            }
        }
        if output.subset_errors.len() > 0 {
            eprintln!("Subset errors:");
            for (&point, errors) in &output.subset_errors {
                print_point(point);
                eprintln!(": {:?}", errors);
            }
        }
        if output.move_errors.len() > 0 {
            eprintln!("Move errors:");
            for (&point, moves) in &output.move_errors {
                print_point(point);
                eprintln!("{:?}", moves);
            }
        }
    }

    let loan_errors = FFIVector__new_vec_pair();
    for (keys, values) in output.errors.iter() {
        let loans_vec = FFIVector__new();
        for loan in values.iter() {
            let loan: usize = loan.into();
            FFIVector__push(loans_vec, loan);
        }
        let point: usize = keys.into();
        let pair = gccrs_ffi::Pair {
            first: point,
            second: loans_vec,
        };
        FFIVector__push_vec_pair(loan_errors, pair);
    }

    let move_errors = FFIVector__new_vec_pair();
    for (keys, values) in output.move_errors.iter() {
        let paths_vec = FFIVector__new();
        for path in values.iter() {
            let path: usize = path.into();
            FFIVector__push(paths_vec, path);
        }
        let point: usize = keys.into();
        let pair = gccrs_ffi::Pair {
            first: point,
            second: paths_vec,
        };
        FFIVector__push_vec_pair(move_errors, pair);
    }

    let subset_errors = FFIVector__new_vec_triple();
    for (key, value) in output.subset_errors.iter() {
        let point: usize = key.into();
        for origin_pair in value.iter() {
            let origin_1: usize = origin_pair.0.into();
            let origin_2: usize = origin_pair.1.into();
            let triple = gccrs_ffi::Triple {
                first: point,
                second: origin_1,
                third: origin_2,
            };
            FFIVector__push_vec_triple(subset_errors, triple);
        }
    }

    return gccrs_ffi::Output {
        loan_errors,
        move_errors,
        subset_errors,
    };
}
