//! Join functionality.

use super::Relation;

/// Performs treefrog leapjoin using a list of leapers.
pub(crate) fn leapjoin<'leap, Tuple: Ord, Val: Ord + 'leap, Result: Ord>(
    source: &[Tuple],
    mut leapers: impl Leapers<'leap, Tuple, Val>,
    mut logic: impl FnMut(&Tuple, &Val) -> Result,
) -> Relation<Result> {
    let mut result = Vec::new(); // temp output storage.
    let mut values = Vec::new(); // temp value storage.

    for tuple in source {
        // Determine which leaper would propose the fewest values.
        let mut min_index = usize::max_value();
        let mut min_count = usize::max_value();
        leapers.for_each_count(tuple, |index, count| {
            if min_count > count {
                min_count = count;
                min_index = index;
            }
        });

        // We had best have at least one relation restricting values.
        assert!(min_count < usize::max_value());

        // If there are values to propose:
        if min_count > 0 {
            // Push the values that `min_index` "proposes" into `values`.
            leapers.propose(tuple, min_index, &mut values);

            // Give other leapers a chance to remove values from
            // anti-joins or filters.
            leapers.intersect(tuple, min_index, &mut values);

            // Push remaining items into result.
            for val in values.drain(..) {
                result.push(logic(tuple, val));
            }
        }
    }

    Relation::from_vec(result)
}

/// Implemented for a tuple of leapers
pub trait Leapers<'leap, Tuple, Val> {
    /// Internal method:
    fn for_each_count(&mut self, tuple: &Tuple, op: impl FnMut(usize, usize));

    /// Internal method:
    fn propose(&mut self, tuple: &Tuple, min_index: usize, values: &mut Vec<&'leap Val>);

    /// Internal method:
    fn intersect(&mut self, tuple: &Tuple, min_index: usize, values: &mut Vec<&'leap Val>);
}

macro_rules! tuple_leapers {
    ($($Ty:ident)*) => {
        #[allow(unused_assignments, non_snake_case)]
        impl<'leap, Tuple, Val, $($Ty),*> Leapers<'leap, Tuple, Val> for ($($Ty,)*)
        where
            $($Ty: Leaper<'leap, Tuple, Val>,)*
        {
            fn for_each_count(&mut self, tuple: &Tuple, mut op: impl FnMut(usize, usize)) {
                let ($($Ty,)*) = self;
                let mut index = 0;
                $(
                    let count = $Ty.count(tuple);
                    op(index, count);
                    index += 1;
                )*
            }

            fn propose(&mut self, tuple: &Tuple, min_index: usize, values: &mut Vec<&'leap Val>) {
                let ($($Ty,)*) = self;
                let mut index = 0;
                $(
                    if min_index == index {
                        return $Ty.propose(tuple, values);
                    }
                    index += 1;
                )*
                    panic!("no match found for min_index={}", min_index);
            }

            fn intersect(&mut self, tuple: &Tuple, min_index: usize, values: &mut Vec<&'leap Val>) {
                let ($($Ty,)*) = self;
                let mut index = 0;
                $(
                    if min_index != index {
                        $Ty.intersect(tuple, values);
                    }
                    index += 1;
                )*
            }
        }
    }
}

tuple_leapers!(A B);
tuple_leapers!(A B C);
tuple_leapers!(A B C D);
tuple_leapers!(A B C D E);
tuple_leapers!(A B C D E F);
tuple_leapers!(A B C D E F G);

/// Methods to support treefrog leapjoin.
pub trait Leaper<'leap, Tuple, Val> {
    /// Estimates the number of proposed values.
    fn count(&mut self, prefix: &Tuple) -> usize;
    /// Populates `values` with proposed values.
    fn propose(&mut self, prefix: &Tuple, values: &mut Vec<&'leap Val>);
    /// Restricts `values` to proposed values.
    fn intersect(&mut self, prefix: &Tuple, values: &mut Vec<&'leap Val>);
}

pub(crate) mod filters {
    use super::Leaper;
    use super::Leapers;

    /// A treefrog leaper that tests each of the tuples from the main
    /// input (the "prefix"). Use like `PrefixFilter::from(|tuple|
    /// ...)`; if the closure returns true, then the tuple is
    /// retained, else it will be ignored. This leaper can be used in
    /// isolation in which case it just acts like a filter on the
    /// input (the "proposed value" will be `()` type).
    pub struct PrefixFilter<Tuple, Func: Fn(&Tuple) -> bool> {
        phantom: ::std::marker::PhantomData<Tuple>,
        predicate: Func,
    }

    impl<'leap, Tuple, Func> PrefixFilter<Tuple, Func>
    where
        Func: Fn(&Tuple) -> bool,
    {
        /// Creates a new filter based on the prefix
        pub fn from(predicate: Func) -> Self {
            PrefixFilter {
                phantom: ::std::marker::PhantomData,
                predicate,
            }
        }
    }

    impl<'leap, Tuple, Val, Func> Leaper<'leap, Tuple, Val> for PrefixFilter<Tuple, Func>
    where
        Func: Fn(&Tuple) -> bool,
    {
        /// Estimates the number of proposed values.
        fn count(&mut self, prefix: &Tuple) -> usize {
            if (self.predicate)(prefix) {
                usize::max_value()
            } else {
                0
            }
        }
        /// Populates `values` with proposed values.
        fn propose(&mut self, _prefix: &Tuple, _values: &mut Vec<&'leap Val>) {
            panic!("PrefixFilter::propose(): variable apparently unbound");
        }
        /// Restricts `values` to proposed values.
        fn intersect(&mut self, _prefix: &Tuple, _values: &mut Vec<&'leap Val>) {
            // We can only be here if we returned max_value() above.
        }
    }

    impl<'leap, Tuple, Func> Leapers<'leap, Tuple, ()> for PrefixFilter<Tuple, Func>
    where
        Func: Fn(&Tuple) -> bool,
    {
        fn for_each_count(&mut self, tuple: &Tuple, mut op: impl FnMut(usize, usize)) {
            if <Self as Leaper<'_, Tuple, ()>>::count(self, tuple) == 0 {
                op(0, 0)
            } else {
                // we will "propose" the `()` value if the predicate applies
                op(0, 1)
            }
        }

        fn propose(&mut self, _: &Tuple, min_index: usize, values: &mut Vec<&'leap ()>) {
            assert_eq!(min_index, 0);
            values.push(&());
        }

        fn intersect(&mut self, _: &Tuple, min_index: usize, values: &mut Vec<&'leap ()>) {
            assert_eq!(min_index, 0);
            assert_eq!(values.len(), 1);
        }
    }

    /// A treefrog leaper based on a predicate of prefix and value.
    /// Use like `ValueFilter::from(|tuple, value| ...)`. The closure
    /// should return true if `value` ought to be retained. The
    /// `value` will be a value proposed elsewhere by an `extend_with`
    /// leaper.
    ///
    /// This leaper cannot be used in isolation, it must be combined
    /// with other leapers.
    pub struct ValueFilter<Tuple, Val, Func: Fn(&Tuple, &Val) -> bool> {
        phantom: ::std::marker::PhantomData<(Tuple, Val)>,
        predicate: Func,
    }

    impl<'leap, Tuple, Val, Func> ValueFilter<Tuple, Val, Func>
    where
        Func: Fn(&Tuple, &Val) -> bool,
    {
        /// Creates a new filter based on the prefix
        pub fn from(predicate: Func) -> Self {
            ValueFilter {
                phantom: ::std::marker::PhantomData,
                predicate,
            }
        }
    }

    impl<'leap, Tuple, Val, Func> Leaper<'leap, Tuple, Val> for ValueFilter<Tuple, Val, Func>
    where
        Func: Fn(&Tuple, &Val) -> bool,
    {
        /// Estimates the number of proposed values.
        fn count(&mut self, _prefix: &Tuple) -> usize {
            usize::max_value()
        }
        /// Populates `values` with proposed values.
        fn propose(&mut self, _prefix: &Tuple, _values: &mut Vec<&'leap Val>) {
            panic!("PrefixFilter::propose(): variable apparently unbound");
        }
        /// Restricts `values` to proposed values.
        fn intersect(&mut self, prefix: &Tuple, values: &mut Vec<&'leap Val>) {
            values.retain(|val| (self.predicate)(prefix, val));
        }
    }

}

/// Extension method for relations.
pub trait RelationLeaper<Key: Ord, Val: Ord> {
    /// Extend with `Val` using the elements of the relation.
    fn extend_with<'leap, Tuple: Ord, Func: Fn(&Tuple) -> Key>(
        &'leap self,
        key_func: Func,
    ) -> extend_with::ExtendWith<'leap, Key, Val, Tuple, Func>
    where
        Key: 'leap,
        Val: 'leap;
    /// Extend with `Val` using the complement of the relation.
    fn extend_anti<'leap, Tuple: Ord, Func: Fn(&Tuple) -> Key>(
        &'leap self,
        key_func: Func,
    ) -> extend_anti::ExtendAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: 'leap,
        Val: 'leap;
    /// Extend with any value if tuple is present in relation.
    fn filter_with<'leap, Tuple: Ord, Func: Fn(&Tuple) -> (Key, Val)>(
        &'leap self,
        key_func: Func,
    ) -> filter_with::FilterWith<'leap, Key, Val, Tuple, Func>
    where
        Key: 'leap,
        Val: 'leap;
    /// Extend with any value if tuple is absent from relation.
    fn filter_anti<'leap, Tuple: Ord, Func: Fn(&Tuple) -> (Key, Val)>(
        &'leap self,
        key_func: Func,
    ) -> filter_anti::FilterAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: 'leap,
        Val: 'leap;
}

impl<Key: Ord, Val: Ord> RelationLeaper<Key, Val> for Relation<(Key, Val)> {
    fn extend_with<'leap, Tuple: Ord, Func: Fn(&Tuple) -> Key>(
        &'leap self,
        key_func: Func,
    ) -> extend_with::ExtendWith<'leap, Key, Val, Tuple, Func>
    where
        Key: 'leap,
        Val: 'leap,
    {
        extend_with::ExtendWith::from(self, key_func)
    }
    fn extend_anti<'leap, Tuple: Ord, Func: Fn(&Tuple) -> Key>(
        &'leap self,
        key_func: Func,
    ) -> extend_anti::ExtendAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: 'leap,
        Val: 'leap,
    {
        extend_anti::ExtendAnti::from(self, key_func)
    }
    fn filter_with<'leap, Tuple: Ord, Func: Fn(&Tuple) -> (Key, Val)>(
        &'leap self,
        key_func: Func,
    ) -> filter_with::FilterWith<'leap, Key, Val, Tuple, Func>
    where
        Key: 'leap,
        Val: 'leap,
    {
        filter_with::FilterWith::from(self, key_func)
    }
    fn filter_anti<'leap, Tuple: Ord, Func: Fn(&Tuple) -> (Key, Val)>(
        &'leap self,
        key_func: Func,
    ) -> filter_anti::FilterAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: 'leap,
        Val: 'leap,
    {
        filter_anti::FilterAnti::from(self, key_func)
    }
}

pub(crate) mod extend_with {
    use super::{binary_search, Leaper, Leapers, Relation};
    use crate::join::gallop;

    /// Wraps a Relation<Tuple> as a leaper.
    pub struct ExtendWith<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> Key,
    {
        relation: &'leap Relation<(Key, Val)>,
        start: usize,
        end: usize,
        key_func: Func,
        phantom: ::std::marker::PhantomData<Tuple>,
    }

    impl<'leap, Key, Val, Tuple, Func> ExtendWith<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> Key,
    {
        /// Constructs a ExtendWith from a relation and key and value function.
        pub fn from(relation: &'leap Relation<(Key, Val)>, key_func: Func) -> Self {
            ExtendWith {
                relation,
                start: 0,
                end: 0,
                key_func,
                phantom: ::std::marker::PhantomData,
            }
        }
    }

    impl<'leap, Key, Val, Tuple, Func> Leaper<'leap, Tuple, Val>
        for ExtendWith<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> Key,
    {
        fn count(&mut self, prefix: &Tuple) -> usize {
            let key = (self.key_func)(prefix);
            self.start = binary_search(&self.relation[..], |x| &x.0 < &key);
            let slice1 = &self.relation[self.start..];
            let slice2 = gallop(slice1, |x| &x.0 <= &key);
            self.end = self.relation.len() - slice2.len();
            slice1.len() - slice2.len()
        }
        fn propose(&mut self, _prefix: &Tuple, values: &mut Vec<&'leap Val>) {
            let slice = &self.relation[self.start..self.end];
            values.extend(slice.iter().map(|&(_, ref val)| val));
        }
        fn intersect(&mut self, _prefix: &Tuple, values: &mut Vec<&'leap Val>) {
            let mut slice = &self.relation[self.start..self.end];
            values.retain(|v| {
                slice = gallop(slice, |kv| &kv.1 < v);
                slice.get(0).map(|kv| &kv.1) == Some(v)
            });
        }
    }

    impl<'leap, Key, Val, Tuple, Func> Leapers<'leap, Tuple, Val>
        for ExtendWith<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> Key,
    {
        fn for_each_count(&mut self, tuple: &Tuple, mut op: impl FnMut(usize, usize)) {
            op(0, self.count(tuple))
        }

        fn propose(&mut self, tuple: &Tuple, min_index: usize, values: &mut Vec<&'leap Val>) {
            assert_eq!(min_index, 0);
            Leaper::propose(self, tuple, values);
        }

        fn intersect(&mut self, _: &Tuple, min_index: usize, _: &mut Vec<&'leap Val>) {
            assert_eq!(min_index, 0);
        }
    }
}

pub(crate) mod extend_anti {
    use super::{binary_search, Leaper, Relation};
    use crate::join::gallop;

    /// Wraps a Relation<Tuple> as a leaper.
    pub struct ExtendAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> Key,
    {
        relation: &'leap Relation<(Key, Val)>,
        key_func: Func,
        phantom: ::std::marker::PhantomData<Tuple>,
    }

    impl<'leap, Key, Val, Tuple, Func> ExtendAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> Key,
    {
        /// Constructs a ExtendAnti from a relation and key and value function.
        pub fn from(relation: &'leap Relation<(Key, Val)>, key_func: Func) -> Self {
            ExtendAnti {
                relation,
                key_func,
                phantom: ::std::marker::PhantomData,
            }
        }
    }

    impl<'leap, Key: Ord, Val: Ord + 'leap, Tuple: Ord, Func> Leaper<'leap, Tuple, Val>
        for ExtendAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> Key,
    {
        fn count(&mut self, _prefix: &Tuple) -> usize {
            usize::max_value()
        }
        fn propose(&mut self, _prefix: &Tuple, _values: &mut Vec<&'leap Val>) {
            panic!("ExtendAnti::propose(): variable apparently unbound.");
        }
        fn intersect(&mut self, prefix: &Tuple, values: &mut Vec<&'leap Val>) {
            let key = (self.key_func)(prefix);
            let start = binary_search(&self.relation[..], |x| &x.0 < &key);
            let slice1 = &self.relation[start..];
            let slice2 = gallop(slice1, |x| &x.0 <= &key);
            let mut slice = &slice1[..(slice1.len() - slice2.len())];
            if !slice.is_empty() {
                values.retain(|v| {
                    slice = gallop(slice, |kv| &kv.1 < v);
                    slice.get(0).map(|kv| &kv.1) != Some(v)
                });
            }
        }
    }
}

pub(crate) mod filter_with {

    use super::{Leaper, Leapers, Relation};

    /// Wraps a Relation<Tuple> as a leaper.
    pub struct FilterWith<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> (Key, Val),
    {
        relation: &'leap Relation<(Key, Val)>,
        key_func: Func,
        phantom: ::std::marker::PhantomData<Tuple>,
    }

    impl<'leap, Key, Val, Tuple, Func> FilterWith<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> (Key, Val),
    {
        /// Constructs a FilterWith from a relation and key and value function.
        pub fn from(relation: &'leap Relation<(Key, Val)>, key_func: Func) -> Self {
            FilterWith {
                relation,
                key_func,
                phantom: ::std::marker::PhantomData,
            }
        }
    }

    impl<'leap, Key, Val, Val2, Tuple, Func> Leaper<'leap, Tuple, Val2>
        for FilterWith<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> (Key, Val),
    {
        fn count(&mut self, prefix: &Tuple) -> usize {
            let key_val = (self.key_func)(prefix);
            if self.relation.binary_search(&key_val).is_ok() {
                usize::max_value()
            } else {
                0
            }
        }
        fn propose(&mut self, _prefix: &Tuple, _values: &mut Vec<&'leap Val2>) {
            panic!("FilterWith::propose(): variable apparently unbound.");
        }
        fn intersect(&mut self, _prefix: &Tuple, _values: &mut Vec<&'leap Val2>) {
            // Only here because we didn't return zero above, right?
        }
    }

    impl<'leap, Key, Val, Tuple, Func> Leapers<'leap, Tuple, ()>
        for FilterWith<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> (Key, Val),
    {
        fn for_each_count(&mut self, tuple: &Tuple, mut op: impl FnMut(usize, usize)) {
            if <Self as Leaper<Tuple, ()>>::count(self, tuple) == 0 {
                op(0, 0)
            } else {
                op(0, 1)
            }
        }

        fn propose(&mut self, _: &Tuple, min_index: usize, values: &mut Vec<&'leap ()>) {
            assert_eq!(min_index, 0);
            values.push(&());
        }

        fn intersect(&mut self, _: &Tuple, min_index: usize, values: &mut Vec<&'leap ()>) {
            assert_eq!(min_index, 0);
            assert_eq!(values.len(), 1);
        }
    }
}

pub(crate) mod filter_anti {

    use super::{Leaper, Leapers, Relation};

    /// Wraps a Relation<Tuple> as a leaper.
    pub struct FilterAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> (Key, Val),
    {
        relation: &'leap Relation<(Key, Val)>,
        key_func: Func,
        phantom: ::std::marker::PhantomData<Tuple>,
    }

    impl<'leap, Key, Val, Tuple, Func> FilterAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> (Key, Val),
    {
        /// Constructs a FilterAnti from a relation and key and value function.
        pub fn from(relation: &'leap Relation<(Key, Val)>, key_func: Func) -> Self {
            FilterAnti {
                relation,
                key_func,
                phantom: ::std::marker::PhantomData,
            }
        }
    }

    impl<'leap, Key: Ord, Val: Ord + 'leap, Val2, Tuple: Ord, Func> Leaper<'leap, Tuple, Val2>
        for FilterAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> (Key, Val),
    {
        fn count(&mut self, prefix: &Tuple) -> usize {
            let key_val = (self.key_func)(prefix);
            if self.relation.binary_search(&key_val).is_ok() {
                0
            } else {
                usize::max_value()
            }
        }
        fn propose(&mut self, _prefix: &Tuple, _values: &mut Vec<&'leap Val2>) {
            panic!("FilterAnti::propose(): variable apparently unbound.");
        }
        fn intersect(&mut self, _prefix: &Tuple, _values: &mut Vec<&'leap Val2>) {
            // Only here because we didn't return zero above, right?
        }
    }

    impl<'leap, Key, Val, Tuple, Func> Leapers<'leap, Tuple, ()>
        for FilterAnti<'leap, Key, Val, Tuple, Func>
    where
        Key: Ord + 'leap,
        Val: Ord + 'leap,
        Tuple: Ord,
        Func: Fn(&Tuple) -> (Key, Val),
    {
        fn for_each_count(&mut self, tuple: &Tuple, mut op: impl FnMut(usize, usize)) {
            if <Self as Leaper<Tuple, ()>>::count(self, tuple) == 0 {
                op(0, 0)
            } else {
                op(0, 1)
            }
        }

        fn propose(&mut self, _: &Tuple, min_index: usize, values: &mut Vec<&'leap ()>) {
            // We only get here if `tuple` is *not* a member of `self.relation`
            assert_eq!(min_index, 0);
            values.push(&());
        }

        fn intersect(&mut self, _: &Tuple, min_index: usize, values: &mut Vec<&'leap ()>) {
            // We only get here if `tuple` is not a member of `self.relation`
            assert_eq!(min_index, 0);
            assert_eq!(values.len(), 1);
        }
    }
}

fn binary_search<T>(slice: &[T], mut cmp: impl FnMut(&T) -> bool) -> usize {
    // we maintain the invariant that `lo` many elements of `slice` satisfy `cmp`.
    // `hi` is maintained at the first element we know does not satisfy `cmp`.

    let mut hi = slice.len();
    let mut lo = 0;
    while lo < hi {
        let mid = lo + (hi - lo) / 2;
        if cmp(&slice[mid]) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    lo
}
