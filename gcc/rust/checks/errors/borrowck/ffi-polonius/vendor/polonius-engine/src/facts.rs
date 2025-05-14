use std::fmt::Debug;
use std::hash::Hash;

/// The "facts" which are the basis of the NLL borrow analysis.
#[derive(Clone, Debug)]
pub struct AllFacts<T: FactTypes> {
    /// `loan_issued_at(origin, loan, point)` indicates that the `loan` was "issued"
    /// at the given `point`, creating a reference with the `origin`.
    /// Effectively, `origin` may refer to data from `loan` starting at `point` (this is usually
    /// the point *after* a borrow rvalue).
    pub loan_issued_at: Vec<(T::Origin, T::Loan, T::Point)>,

    /// `universal_region(origin)` -- this is a "free region" within fn body
    pub universal_region: Vec<T::Origin>,

    /// `cfg_edge(point1, point2)` for each edge `point1 -> point2` in the control flow
    pub cfg_edge: Vec<(T::Point, T::Point)>,

    /// `loan_killed_at(loan, point)` when some prefix of the path borrowed at `loan`
    /// is assigned at `point`.
    /// Indicates that the path borrowed by the `loan` has changed in some way that the loan no
    /// longer needs to be tracked. (In particular, mutations to the path that was borrowed
    /// no longer invalidate the loan)
    pub loan_killed_at: Vec<(T::Loan, T::Point)>,

    /// `subset_base(origin1, origin2, point)` when we require `origin1@point: origin2@point`.
    /// Indicates that `origin1 <= origin2` -- i.e., the set of loans in `origin1` are a subset
    /// of those in `origin2`.
    pub subset_base: Vec<(T::Origin, T::Origin, T::Point)>,

    /// `loan_invalidated_at(point, loan)` indicates that the `loan` is invalidated by some action
    /// taking place at `point`; if any origin that references this loan is live, this is an error.
    pub loan_invalidated_at: Vec<(T::Point, T::Loan)>,

    /// `var_used_at(var, point)` when the variable `var` is used for anything
    /// but a drop at `point`
    pub var_used_at: Vec<(T::Variable, T::Point)>,

    /// `var_defined_at(var, point)` when the variable `var` is overwritten at `point`
    pub var_defined_at: Vec<(T::Variable, T::Point)>,

    /// `var_dropped_at(var, point)` when the variable `var` is used in a drop at `point`
    pub var_dropped_at: Vec<(T::Variable, T::Point)>,

    /// `use_of_var_derefs_origin(variable, origin)`: References with the given
    /// `origin` may be dereferenced when the `variable` is used.
    ///
    /// In rustc, we generate this whenever the type of the variable includes the
    /// given origin.
    pub use_of_var_derefs_origin: Vec<(T::Variable, T::Origin)>,

    /// `drop_of_var_derefs_origin(var, origin)` when the type of `var` includes
    /// the `origin` and uses it when dropping
    pub drop_of_var_derefs_origin: Vec<(T::Variable, T::Origin)>,

    /// `child_path(child, parent)` when the path `child` is the direct child of
    /// `parent`, e.g. `child_path(x.y, x)`, but not `child_path(x.y.z, x)`.
    pub child_path: Vec<(T::Path, T::Path)>,

    /// `path_is_var(path, var)` the root path `path` starting in variable `var`.
    pub path_is_var: Vec<(T::Path, T::Variable)>,

    /// `path_assigned_at_base(path, point)` when the `path` was initialized at point
    /// `point`. This fact is only emitted for a prefix `path`, and not for the
    /// implicit initialization of all of `path`'s children. E.g. a statement like
    /// `x.y = 3` at `point` would give the fact `path_assigned_at_base(x.y, point)` (but
    /// neither `path_assigned_at_base(x.y.z, point)` nor `path_assigned_at_base(x, point)`).
    pub path_assigned_at_base: Vec<(T::Path, T::Point)>,

    /// `path_moved_at_base(path, point)` when the `path` was moved at `point`. The
    /// same logic is applied as for `path_assigned_at_base` above.
    pub path_moved_at_base: Vec<(T::Path, T::Point)>,

    /// `path_accessed_at_base(path, point)` when the `path` was accessed at point
    /// `point`. The same logic as for `path_assigned_at_base` and `path_moved_at_base` applies.
    pub path_accessed_at_base: Vec<(T::Path, T::Point)>,

    /// These reflect the `'a: 'b` relations that are either declared by the user on function
    /// declarations or which are inferred via implied bounds.
    /// For example: `fn foo<'a, 'b: 'a, 'c>(x: &'c &'a u32)` would have two entries:
    /// - one for the user-supplied subset `'b: 'a`
    /// - and one for the `'a: 'c` implied bound from the `x` parameter,
    /// (note that the transitive relation `'b: 'c` is not necessarily included
    /// explicitly, but rather inferred by polonius).
    pub known_placeholder_subset: Vec<(T::Origin, T::Origin)>,

    /// `placeholder(origin, loan)` describes a placeholder `origin`, with its associated
    ///  placeholder `loan`.
    pub placeholder: Vec<(T::Origin, T::Loan)>,
}

impl<T: FactTypes> Default for AllFacts<T> {
    fn default() -> Self {
        AllFacts {
            loan_issued_at: Vec::default(),
            universal_region: Vec::default(),
            cfg_edge: Vec::default(),
            loan_killed_at: Vec::default(),
            subset_base: Vec::default(),
            loan_invalidated_at: Vec::default(),
            var_used_at: Vec::default(),
            var_defined_at: Vec::default(),
            var_dropped_at: Vec::default(),
            use_of_var_derefs_origin: Vec::default(),
            drop_of_var_derefs_origin: Vec::default(),
            child_path: Vec::default(),
            path_is_var: Vec::default(),
            path_assigned_at_base: Vec::default(),
            path_moved_at_base: Vec::default(),
            path_accessed_at_base: Vec::default(),
            known_placeholder_subset: Vec::default(),
            placeholder: Vec::default(),
        }
    }
}

pub trait Atom:
    From<usize> + Into<usize> + Copy + Clone + Debug + Eq + Ord + Hash + 'static
{
    fn index(self) -> usize;
}

pub trait FactTypes: Copy + Clone + Debug {
    type Origin: Atom;
    type Loan: Atom;
    type Point: Atom;
    type Variable: Atom;
    type Path: Atom;
}
