pub type Origin = usize;
pub type Loan = usize;
pub type Point = usize;
pub type Variable = usize;
pub type Path = usize;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Pair<T1, T2> {
    pub first: T1,
    pub second: T2,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Triple<T1, T2, T3> {
    pub first: T1,
    pub second: T2,
    pub third: T3,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Slice<T> {
    pub len: usize,
    pub data: *const T,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct FactsView {
    pub loan_issued_at: Slice<Triple<Origin, Loan, Point>>,
    pub universal_region: Slice<Origin>,
    pub cfg_edge: Slice<Pair<Point, Point>>,
    pub loan_killed_at: Slice<Pair<Loan, Point>>,
    pub subset_base: Slice<Triple<Origin, Origin, Point>>,
    pub loan_invalidated_at: Slice<Pair<Point, Loan>>,
    pub var_used_at: Slice<Pair<Variable, Point>>,
    pub var_defined_at: Slice<Pair<Variable, Point>>,
    pub var_dropped_at: Slice<Pair<Variable, Point>>,
    pub use_of_var_derefs_origin: Slice<Pair<Variable, Origin>>,
    pub drop_of_var_derefs_origin: Slice<Pair<Variable, Origin>>,
    pub child_path: Slice<Pair<Path, Path>>,
    pub path_is_var: Slice<Pair<Path, Variable>>,
    pub path_assigned_at_base: Slice<Pair<Path, Point>>,
    pub path_moved_at_base: Slice<Pair<Path, Point>>,
    pub path_accessed_at_base: Slice<Pair<Path, Point>>,
    pub known_placeholder_subset: Slice<Pair<Origin, Origin>>,
    pub placeholder: Slice<Pair<Origin, Loan>>,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct Output {
    pub loan_errors: *mut FFIVector,
    pub move_errors: *mut FFIVector,
    pub subset_errors: *mut FFIVector,
}
