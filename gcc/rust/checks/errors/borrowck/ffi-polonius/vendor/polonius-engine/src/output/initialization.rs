use std::time::Instant;

use crate::facts::FactTypes;
use crate::output::{InitializationContext, Output};

use datafrog::{Iteration, Relation, RelationLeaper};

// This represents the output of an intermediate elaboration step (step 1).
struct TransitivePaths<T: FactTypes> {
    path_moved_at: Relation<(T::Path, T::Point)>,
    path_assigned_at: Relation<(T::Path, T::Point)>,
    path_accessed_at: Relation<(T::Path, T::Point)>,
    path_begins_with_var: Relation<(T::Path, T::Variable)>,
}

struct InitializationStatus<T: FactTypes> {
    var_maybe_partly_initialized_on_exit: Relation<(T::Variable, T::Point)>,
    move_error: Relation<(T::Path, T::Point)>,
}

pub(super) struct InitializationResult<T: FactTypes>(
    pub(super) Relation<(T::Variable, T::Point)>,
    pub(super) Relation<(T::Path, T::Point)>,
);

// Step 1: compute transitive closures of path operations. This would elaborate,
// for example, an access to x into an access to x.f, x.f.0, etc. We do this for:
// - access to a path
// - initialization of a path
// - moves of a path
// FIXME: transitive rooting in a variable (path_begins_with_var)
// Note that this step may not be entirely necessary!
fn compute_transitive_paths<T: FactTypes>(
    child_path: Vec<(T::Path, T::Path)>,
    path_assigned_at_base: Vec<(T::Path, T::Point)>,
    path_moved_at_base: Vec<(T::Path, T::Point)>,
    path_accessed_at_base: Vec<(T::Path, T::Point)>,
    path_is_var: Vec<(T::Path, T::Variable)>,
) -> TransitivePaths<T> {
    let mut iteration = Iteration::new();
    let child_path: Relation<(T::Path, T::Path)> = child_path.into();

    let ancestor_path = iteration.variable::<(T::Path, T::Path)>("ancestor");

    // These are the actual targets:
    let path_moved_at = iteration.variable::<(T::Path, T::Point)>("path_moved_at");
    let path_assigned_at = iteration.variable::<(T::Path, T::Point)>("path_initialized_at");
    let path_accessed_at = iteration.variable::<(T::Path, T::Point)>("path_accessed_at");
    let path_begins_with_var = iteration.variable::<(T::Path, T::Variable)>("path_begins_with_var");

    // ancestor_path(Parent, Child) :- child_path(Child, Parent).
    ancestor_path.extend(child_path.iter().map(|&(child, parent)| (parent, child)));

    // path_moved_at(Path, Point) :- path_moved_at_base(Path, Point).
    path_moved_at.insert(path_moved_at_base.into());

    // path_assigned_at(Path, Point) :- path_assigned_at_base(Path, Point).
    path_assigned_at.insert(path_assigned_at_base.into());

    // path_accessed_at(Path, Point) :- path_accessed_at_base(Path, Point).
    path_accessed_at.insert(path_accessed_at_base.into());

    // path_begins_with_var(Path, Var) :- path_is_var(Path, Var).
    path_begins_with_var.insert(path_is_var.into());

    while iteration.changed() {
        // ancestor_path(Grandparent, Child) :-
        //    ancestor_path(Parent, Child),
        //    child_path(Parent, Grandparent).
        ancestor_path.from_join(
            &ancestor_path,
            &child_path,
            |&_parent, &child, &grandparent| (grandparent, child),
        );

        // moving a path moves its children
        // path_moved_at(Child, Point) :-
        //     path_moved_at(Parent, Point),
        //     ancestor_path(Parent, Child).
        path_moved_at.from_join(&path_moved_at, &ancestor_path, |&_parent, &p, &child| {
            (child, p)
        });

        // initialising x at p initialises all x:s children
        // path_assigned_at(Child, point) :-
        //     path_assigned_at(Parent, point),
        //     ancestor_path(Parent, Child).
        path_assigned_at.from_join(&path_assigned_at, &ancestor_path, |&_parent, &p, &child| {
            (child, p)
        });

        // accessing x at p accesses all x:s children at p (actually,
        // accesses should be maximally precise and this shouldn't happen?)
        // path_accessed_at(Child, point) :-
        //   path_accessed_at(Parent, point),
        //   ancestor_path(Parent, Child).
        path_accessed_at.from_join(&path_accessed_at, &ancestor_path, |&_parent, &p, &child| {
            (child, p)
        });

        // path_begins_with_var(Child, Var) :-
        //   path_begins_with_var(Parent, Var)
        //   ancestor_path(Parent, Child).
        path_begins_with_var.from_join(
            &path_begins_with_var,
            &ancestor_path,
            |&_parent, &var, &child| (child, var),
        );
    }

    TransitivePaths {
        path_assigned_at: path_assigned_at.complete(),
        path_moved_at: path_moved_at.complete(),
        path_accessed_at: path_accessed_at.complete(),
        path_begins_with_var: path_begins_with_var.complete(),
    }
}

// Step 2: Compute path initialization and deinitialization across the CFG.
fn compute_move_errors<T: FactTypes>(
    ctx: TransitivePaths<T>,
    cfg_edge: &Relation<(T::Point, T::Point)>,
    output: &mut Output<T>,
) -> InitializationStatus<T> {
    let mut iteration = Iteration::new();
    // Variables

    // var_maybe_partly_initialized_on_exit(var, point): Upon leaving `point`,
    // `var` is partially initialized for some path through the CFG, that is
    // there has been an initialization of var, and var has not been moved in
    // all paths through the CFG.
    let var_maybe_partly_initialized_on_exit =
        iteration.variable::<(T::Variable, T::Point)>("var_maybe_partly_initialized_on_exit");

    // path_maybe_initialized_on_exit(path, point): Upon leaving `point`, the
    // move path `path` is initialized for some path through the CFG.
    let path_maybe_initialized_on_exit =
        iteration.variable::<(T::Path, T::Point)>("path_maybe_initialized_on_exit");

    // path_maybe_uninitialized_on_exit(Path, Point): There exists at least one
    // path through the CFG to Point such that `Path` has been moved out by the
    // time we arrive at `Point` without it being re-initialized for sure.
    let path_maybe_uninitialized_on_exit =
        iteration.variable::<(T::Path, T::Point)>("path_maybe_uninitialized_on_exit");

    // move_error(Path, Point): There is an access to `Path` at `Point`, but
    // `Path` is potentially moved (or never initialised).
    let move_error = iteration.variable::<(T::Path, T::Point)>("move_error");

    // Initial propagation of static relations

    // path_maybe_initialized_on_exit(path, point) :- path_assigned_at(path, point).
    path_maybe_initialized_on_exit.insert(ctx.path_assigned_at.clone());

    // path_maybe_uninitialized_on_exit(path, point) :- path_moved_at(path, point).
    path_maybe_uninitialized_on_exit.insert(ctx.path_moved_at.clone());

    while iteration.changed() {
        // path_maybe_initialized_on_exit(path, point2) :-
        //     path_maybe_initialized_on_exit(path, point1),
        //     cfg_edge(point1, point2),
        //     !path_moved_at(path, point2).
        path_maybe_initialized_on_exit.from_leapjoin(
            &path_maybe_initialized_on_exit,
            (
                cfg_edge.extend_with(|&(_path, point1)| point1),
                ctx.path_moved_at.extend_anti(|&(path, _point1)| path),
            ),
            |&(path, _point1), &point2| (path, point2),
        );

        // path_maybe_uninitialized_on_exit(path, point2) :-
        //     path_maybe_uninitialized_on_exit(path, point1),
        //     cfg_edge(point1, point2)
        //     !path_assigned_at(path, point2).
        path_maybe_uninitialized_on_exit.from_leapjoin(
            &path_maybe_uninitialized_on_exit,
            (
                cfg_edge.extend_with(|&(_path, point1)| point1),
                ctx.path_assigned_at.extend_anti(|&(path, _point1)| path),
            ),
            |&(path, _point1), &point2| (path, point2),
        );

        // var_maybe_partly_initialized_on_exit(var, point) :-
        //     path_maybe_initialized_on_exit(path, point).
        //     path_begins_with_var(path, var).
        var_maybe_partly_initialized_on_exit.from_leapjoin(
            &path_maybe_initialized_on_exit,
            ctx.path_begins_with_var.extend_with(|&(path, _point)| path),
            |&(_path, point), &var| (var, point),
        );

        // move_error(Path, TargetNode) :-
        //   path_maybe_uninitialized_on_exit(Path, SourceNode),
        //   cfg_edge(SourceNode, TargetNode),
        //   path_accessed_at(Path, TargetNode).
        move_error.from_leapjoin(
            &path_maybe_uninitialized_on_exit,
            (
                cfg_edge.extend_with(|&(_path, source_node)| source_node),
                ctx.path_accessed_at
                    .extend_with(|&(path, _source_node)| path),
            ),
            |&(path, _source_node), &target_node| (path, target_node),
        );
    }

    if output.dump_enabled {
        for &(path, location) in path_maybe_initialized_on_exit.complete().iter() {
            output
                .path_maybe_initialized_on_exit
                .entry(location)
                .or_default()
                .push(path);
        }

        for &(path, location) in path_maybe_uninitialized_on_exit.complete().iter() {
            output
                .path_maybe_uninitialized_on_exit
                .entry(location)
                .or_default()
                .push(path);
        }
    }

    InitializationStatus {
        var_maybe_partly_initialized_on_exit: var_maybe_partly_initialized_on_exit.complete(),
        move_error: move_error.complete(),
    }
}

// Compute two things:
//
// - an over-approximation of the initialization of variables. This is used in
//   the origin_live_on_entry computations to determine when a drop may happen; a
//   definitely moved variable would not be actually dropped.
// - move errors.
//
// The process is split into two stages:
//
// 1. Compute the transitive closure of path accesses. That is, accessing `f.a`
//   would access `f.a.b`, etc.
// 2. Use this to compute both paths that may be initialized and paths that may
//   have been deinitialized, which in turn can be used to find move errors (an
//   access to a path that may be deinitialized).
pub(super) fn compute<T: FactTypes>(
    ctx: InitializationContext<T>,
    cfg_edge: &Relation<(T::Point, T::Point)>,
    output: &mut Output<T>,
) -> InitializationResult<T> {
    let timer = Instant::now();

    let transitive_paths = compute_transitive_paths::<T>(
        ctx.child_path,
        ctx.path_assigned_at_base,
        ctx.path_moved_at_base,
        ctx.path_accessed_at_base,
        ctx.path_is_var,
    );
    info!("initialization phase 1 completed: {:?}", timer.elapsed());

    let InitializationStatus {
        var_maybe_partly_initialized_on_exit,
        move_error,
    } = compute_move_errors::<T>(transitive_paths, cfg_edge, output);
    info!(
        "initialization phase 2: {} move errors in {:?}",
        move_error.elements.len(),
        timer.elapsed()
    );

    if output.dump_enabled {
        for &(var, location) in var_maybe_partly_initialized_on_exit.iter() {
            output
                .var_maybe_partly_initialized_on_exit
                .entry(location)
                .or_default()
                .push(var);
        }
    }

    InitializationResult(var_maybe_partly_initialized_on_exit, move_error)
}
