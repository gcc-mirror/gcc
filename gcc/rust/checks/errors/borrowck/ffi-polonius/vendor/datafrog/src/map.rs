//! Map functionality.

use super::{Relation, Variable};

pub(crate) fn map_into<T1: Ord, T2: Ord>(
    input: &Variable<T1>,
    output: &Variable<T2>,
    logic: impl FnMut(&T1) -> T2,
) {
    let results: Vec<T2> = input.recent.borrow().iter().map(logic).collect();

    output.insert(Relation::from_vec(results));
}
