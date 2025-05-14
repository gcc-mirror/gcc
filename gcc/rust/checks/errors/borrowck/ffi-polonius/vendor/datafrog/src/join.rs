//! Join functionality.

use super::{Relation, Variable};
use std::cell::Ref;
use std::ops::Deref;

/// Implements `join`. Note that `input1` must be a variable, but
/// `input2` can be either a variable or a relation. This is necessary
/// because relations have no "recent" tuples, so the fn would be a
/// guaranteed no-op if both arguments were relations.  See also
/// `join_into_relation`.
pub(crate) fn join_into<'me, Key: Ord, Val1: Ord, Val2: Ord, Result: Ord>(
    input1: &Variable<(Key, Val1)>,
    input2: impl JoinInput<'me, (Key, Val2)>,
    output: &Variable<Result>,
    mut logic: impl FnMut(&Key, &Val1, &Val2) -> Result,
) {
    let mut results = Vec::new();

    let recent1 = input1.recent();
    let recent2 = input2.recent();

    {
        // scoped to let `closure` drop borrow of `results`.

        let mut closure = |k: &Key, v1: &Val1, v2: &Val2| results.push(logic(k, v1, v2));

        for batch2 in input2.stable().iter() {
            join_helper(&recent1, &batch2, &mut closure);
        }

        for batch1 in input1.stable().iter() {
            join_helper(&batch1, &recent2, &mut closure);
        }

        join_helper(&recent1, &recent2, &mut closure);
    }

    output.insert(Relation::from_vec(results));
}

/// Join, but for two relations.
pub(crate) fn join_into_relation<'me, Key: Ord, Val1: Ord, Val2: Ord, Result: Ord>(
    input1: &Relation<(Key, Val1)>,
    input2: &Relation<(Key, Val2)>,
    mut logic: impl FnMut(&Key, &Val1, &Val2) -> Result,
) -> Relation<Result> {
    let mut results = Vec::new();

    join_helper(&input1.elements, &input2.elements, |k, v1, v2| {
        results.push(logic(k, v1, v2));
    });

    Relation::from_vec(results)
}

/// Moves all recent tuples from `input1` that are not present in `input2` into `output`.
pub(crate) fn antijoin<'me, Key: Ord, Val: Ord, Result: Ord>(
    input1: impl JoinInput<'me, (Key, Val)>,
    input2: &Relation<Key>,
    mut logic: impl FnMut(&Key, &Val) -> Result,
) -> Relation<Result> {
    let mut tuples2 = &input2[..];

    let results = input1
        .recent()
        .iter()
        .filter(|(ref key, _)| {
            tuples2 = gallop(tuples2, |k| k < key);
            tuples2.first() != Some(key)
        })
        .map(|(ref key, ref val)| logic(key, val))
        .collect::<Vec<_>>();

    Relation::from_vec(results)
}

fn join_helper<K: Ord, V1, V2>(
    mut slice1: &[(K, V1)],
    mut slice2: &[(K, V2)],
    mut result: impl FnMut(&K, &V1, &V2),
) {
    while !slice1.is_empty() && !slice2.is_empty() {
        use std::cmp::Ordering;

        // If the keys match produce tuples, else advance the smaller key until they might.
        match slice1[0].0.cmp(&slice2[0].0) {
            Ordering::Less => {
                slice1 = gallop(slice1, |x| x.0 < slice2[0].0);
            }
            Ordering::Equal => {
                // Determine the number of matching keys in each slice.
                let count1 = slice1.iter().take_while(|x| x.0 == slice1[0].0).count();
                let count2 = slice2.iter().take_while(|x| x.0 == slice2[0].0).count();

                // Produce results from the cross-product of matches.
                for index1 in 0..count1 {
                    for s2 in slice2[..count2].iter() {
                        result(&slice1[0].0, &slice1[index1].1, &s2.1);
                    }
                }

                // Advance slices past this key.
                slice1 = &slice1[count1..];
                slice2 = &slice2[count2..];
            }
            Ordering::Greater => {
                slice2 = gallop(slice2, |x| x.0 < slice1[0].0);
            }
        }
    }
}

pub(crate) fn gallop<T>(mut slice: &[T], mut cmp: impl FnMut(&T) -> bool) -> &[T] {
    // if empty slice, or already >= element, return
    if !slice.is_empty() && cmp(&slice[0]) {
        let mut step = 1;
        while step < slice.len() && cmp(&slice[step]) {
            slice = &slice[step..];
            step <<= 1;
        }

        step >>= 1;
        while step > 0 {
            if step < slice.len() && cmp(&slice[step]) {
                slice = &slice[step..];
            }
            step >>= 1;
        }

        slice = &slice[1..]; // advance one, as we always stayed < value
    }

    slice
}

/// An input that can be used with `from_join`; either a `Variable` or a `Relation`.
pub trait JoinInput<'me, Tuple: Ord>: Copy {
    /// If we are on iteration N of the loop, these are the tuples
    /// added on iteration N-1. (For a `Relation`, this is always an
    /// empty slice.)
    type RecentTuples: Deref<Target = [Tuple]>;

    /// If we are on iteration N of the loop, these are the tuples
    /// added on iteration N - 2 or before. (For a `Relation`, this is
    /// just `self`.)
    type StableTuples: Deref<Target = [Relation<Tuple>]>;

    /// Get the set of recent tuples.
    fn recent(self) -> Self::RecentTuples;

    /// Get the set of stable tuples.
    fn stable(self) -> Self::StableTuples;
}

impl<'me, Tuple: Ord> JoinInput<'me, Tuple> for &'me Variable<Tuple> {
    type RecentTuples = Ref<'me, [Tuple]>;
    type StableTuples = Ref<'me, [Relation<Tuple>]>;

    fn recent(self) -> Self::RecentTuples {
        Ref::map(self.recent.borrow(), |r| &r.elements[..])
    }

    fn stable(self) -> Self::StableTuples {
        Ref::map(self.stable.borrow(), |v| &v[..])
    }
}

impl<'me, Tuple: Ord> JoinInput<'me, Tuple> for &'me Relation<Tuple> {
    type RecentTuples = &'me [Tuple];
    type StableTuples = &'me [Relation<Tuple>];

    fn recent(self) -> Self::RecentTuples {
        &[]
    }

    fn stable(self) -> Self::StableTuples {
        std::slice::from_ref(self)
    }
}
