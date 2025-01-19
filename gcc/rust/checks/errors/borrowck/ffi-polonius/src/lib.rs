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
mod gccrs_ffi_generated;

use polonius_engine::{AllFacts, Atom, FactTypes, Output};
use std::fmt::Debug;
use std::hash::Hash;

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
    let mid = val % 2 == 1;
    let bb = val >> 16;
    let stmt = (val >> 1) & 0xFFFF;
    print!("{}(bb{}[{}])", if mid { "Mid" } else { "Start" }, bb, stmt);
}

/// Run the polonius analysis on the given facts (for a single function).
/// Right now, results are only printed and not propagated back to the gccrs.
#[no_mangle]
pub unsafe extern "C" fn polonius_run(input: gccrs_ffi::FactsView, dump_enabled: bool) {
    let facts = AllFacts::<GccrsFacts>::from(input);
    let output = Output::compute(&facts, polonius_engine::Algorithm::Naive, dump_enabled);

    // FIXME: Temporary output
    println!("Polonius analysis completed. Results:");
    println!("Errors: {:#?}", output.errors);
    println!("Subset error: {:#?}", output.subset_errors);
    println!("Move error: {:#?}", output.move_errors);

    println!("Subsets:");
    let mut subset_vec: Vec<_> = output.subset.iter().collect();
    subset_vec.sort_by_key(|&(point, _)| point);
    for (point, subsets) in subset_vec {
        print_point(*point);
        println!(": {{");
        for (&lhs, rhss) in subsets {
            for &rhs in rhss {
                println!("    {} <= {}", usize::from(lhs), usize::from(rhs));
            }
        }
        println!("}}");
    }
    println!("Subset anywhere: {:#?}", output.subset_anywhere);

    // Print origin live on entry
    println!("Origin live on entry:");
    let mut origin_vec: Vec<_> = output.origin_live_on_entry.iter().collect();
    origin_vec.sort_by_key(|&(point, _)| point);
    for (point, origins) in origin_vec {
        print_point(*point);
        println!(": {{");
        for &origin in origins {
            println!("    {}", usize::from(origin));
        }
        println!("}}");
    }
}
