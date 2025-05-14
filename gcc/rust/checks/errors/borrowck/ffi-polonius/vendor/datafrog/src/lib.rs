//! A lightweight Datalog engine in Rust
//!
//! The intended design is that one has static `Relation` types that are sets
//! of tuples, and `Variable` types that represent monotonically increasing
//! sets of tuples.
//!
//! The types are mostly wrappers around `Vec<Tuple>` indicating sorted-ness,
//! and the intent is that this code can be dropped in the middle of an otherwise
//! normal Rust program, run to completion, and then the results extracted as
//! vectors again.

#![forbid(missing_docs)]

use std::cell::RefCell;
use std::cmp::Ordering;
use std::iter::FromIterator;
use std::rc::Rc;

mod join;
mod map;
mod test;
mod treefrog;
pub use crate::join::JoinInput;
pub use crate::treefrog::{
    extend_anti::ExtendAnti,
    extend_with::ExtendWith,
    filter_anti::FilterAnti,
    filter_with::FilterWith,
    filters::{PrefixFilter, ValueFilter},
    Leaper, Leapers, RelationLeaper,
};

/// A static, ordered list of key-value pairs.
///
/// A relation represents a fixed set of key-value pairs. In many places in a
/// Datalog computation we want to be sure that certain relations are not able
/// to vary (for example, in antijoins).
#[derive(Clone)]
pub struct Relation<Tuple: Ord> {
    /// Sorted list of distinct tuples.
    pub elements: Vec<Tuple>,
}

impl<Tuple: Ord> Relation<Tuple> {
    /// Merges two relations into their union.
    pub fn merge(self, other: Self) -> Self {
        let Relation {
            elements: mut elements1,
        } = self;
        let Relation {
            elements: mut elements2,
        } = other;

        // If one of the element lists is zero-length, we don't need to do any work
        if elements1.is_empty() {
            return Relation {
                elements: elements2,
            };
        }

        if elements2.is_empty() {
            return Relation {
                elements: elements1,
            };
        }

        // Make sure that elements1 starts with the lower element
        // Will not panic since both collections must have at least 1 element at this point
        if elements1[0] > elements2[0] {
            std::mem::swap(&mut elements1, &mut elements2);
        }

        // Fast path for when all the new elements are after the exiting ones
        if elements1[elements1.len() - 1] < elements2[0] {
            elements1.extend(elements2.into_iter());
            // println!("fast path");
            return Relation {
                elements: elements1,
            };
        }

        let mut elements = Vec::with_capacity(elements1.len() + elements2.len());
        let mut elements1 = elements1.drain(..);
        let mut elements2 = elements2.drain(..).peekable();

        elements.push(elements1.next().unwrap());
        if elements.first() == elements2.peek() {
            elements2.next();
        }

        for elem in elements1 {
            while elements2.peek().map(|x| x.cmp(&elem)) == Some(Ordering::Less) {
                elements.push(elements2.next().unwrap());
            }
            if elements2.peek().map(|x| x.cmp(&elem)) == Some(Ordering::Equal) {
                elements2.next();
            }
            elements.push(elem);
        }

        // Finish draining second list
        elements.extend(elements2);

        Relation { elements }
    }

    /// Creates a `Relation` from the elements of the `iterator`.
    ///
    /// Same as the `from_iter` method from `std::iter::FromIterator` trait.
    pub fn from_iter<I>(iterator: I) -> Self
    where
        I: IntoIterator<Item = Tuple>,
    {
        iterator.into_iter().collect()
    }

    /// Creates a `Relation` using the `leapjoin` logic;
    /// see [`Variable::from_leapjoin`]
    pub fn from_leapjoin<'leap, SourceTuple: Ord, Val: Ord + 'leap>(
        source: &Relation<SourceTuple>,
        leapers: impl Leapers<'leap, SourceTuple, Val>,
        logic: impl FnMut(&SourceTuple, &Val) -> Tuple,
    ) -> Self {
        treefrog::leapjoin(&source.elements, leapers, logic)
    }

    /// Creates a `Relation` by joining the values from `input1` and
    /// `input2` and then applying `logic`. Like
    /// [`Variable::from_join`] except for use where the inputs are
    /// not varying across iterations.
    pub fn from_join<Key: Ord, Val1: Ord, Val2: Ord>(
        input1: &Relation<(Key, Val1)>,
        input2: &Relation<(Key, Val2)>,
        logic: impl FnMut(&Key, &Val1, &Val2) -> Tuple,
    ) -> Self {
        join::join_into_relation(input1, input2, logic)
    }

    /// Creates a `Relation` by removing all values from `input1` that
    /// share a key with `input2`, and then transforming the resulting
    /// tuples with the `logic` closure. Like
    /// [`Variable::from_antijoin`] except for use where the inputs
    /// are not varying across iterations.
    pub fn from_antijoin<Key: Ord, Val1: Ord>(
        input1: &Relation<(Key, Val1)>,
        input2: &Relation<Key>,
        logic: impl FnMut(&Key, &Val1) -> Tuple,
    ) -> Self {
        join::antijoin(input1, input2, logic)
    }

    /// Construct a new relation by mapping another one. Equivalent to
    /// creating an iterator but perhaps more convenient. Analogous to
    /// `Variable::from_map`.
    pub fn from_map<T2: Ord>(input: &Relation<T2>, logic: impl FnMut(&T2) -> Tuple) -> Self {
        input.iter().map(logic).collect()
    }

    /// Creates a `Relation` from a vector of tuples.
    pub fn from_vec(mut elements: Vec<Tuple>) -> Self {
        elements.sort();
        elements.dedup();
        Relation { elements }
    }
}

impl<Tuple: Ord> From<Vec<Tuple>> for Relation<Tuple> {
    fn from(iterator: Vec<Tuple>) -> Self {
        Self::from_vec(iterator)
    }
}

impl<Tuple: Ord> FromIterator<Tuple> for Relation<Tuple> {
    fn from_iter<I>(iterator: I) -> Self
    where
        I: IntoIterator<Item = Tuple>,
    {
        Relation::from_vec(iterator.into_iter().collect())
    }
}

impl<'tuple, Tuple: 'tuple + Copy + Ord> FromIterator<&'tuple Tuple> for Relation<Tuple> {
    fn from_iter<I>(iterator: I) -> Self
    where
        I: IntoIterator<Item = &'tuple Tuple>,
    {
        Relation::from_vec(iterator.into_iter().cloned().collect())
    }
}

impl<Tuple: Ord> std::ops::Deref for Relation<Tuple> {
    type Target = [Tuple];
    fn deref(&self) -> &Self::Target {
        &self.elements[..]
    }
}

/// An iterative context for recursive evaluation.
///
/// An `Iteration` tracks monotonic variables, and monitors their progress.
/// It can inform the user if they have ceased changing, at which point the
/// computation should be done.
pub struct Iteration {
    variables: Vec<Box<dyn VariableTrait>>,
}

impl Iteration {
    /// Create a new iterative context.
    pub fn new() -> Self {
        Iteration {
            variables: Vec::new(),
        }
    }
    /// Reports whether any of the monitored variables have changed since
    /// the most recent call.
    pub fn changed(&mut self) -> bool {
        let mut result = false;
        for variable in self.variables.iter_mut() {
            if variable.changed() {
                result = true;
            }
        }
        result
    }
    /// Creates a new named variable associated with the iterative context.
    pub fn variable<Tuple: Ord + 'static>(&mut self, name: &str) -> Variable<Tuple> {
        let variable = Variable::new(name);
        self.variables.push(Box::new(variable.clone()));
        variable
    }
    /// Creates a new named variable associated with the iterative context.
    ///
    /// This variable will not be maintained distinctly, and may advertise tuples as
    /// recent multiple times (perhaps unboundedly many times).
    pub fn variable_indistinct<Tuple: Ord + 'static>(&mut self, name: &str) -> Variable<Tuple> {
        let mut variable = Variable::new(name);
        variable.distinct = false;
        self.variables.push(Box::new(variable.clone()));
        variable
    }
}

/// A type that can report on whether it has changed.
trait VariableTrait {
    /// Reports whether the variable has changed since it was last asked.
    fn changed(&mut self) -> bool;
}

/// An monotonically increasing set of `Tuple`s.
///
/// There are three stages in the lifecycle of a tuple:
///
///   1. A tuple is added to `self.to_add`, but is not yet visible externally.
///   2. Newly added tuples are then promoted to `self.recent` for one iteration.
///   3. After one iteration, recent tuples are moved to `self.tuples` for posterity.
///
/// Each time `self.changed()` is called, the `recent` relation is folded into `tuples`,
/// and the `to_add` relations are merged, potentially deduplicated against `tuples`, and
/// then made  `recent`. This way, across calls to `changed()` all added tuples are in
/// `recent` at least once and eventually all are in `tuples`.
///
/// A `Variable` may optionally be instructed not to de-duplicate its tuples, for reasons
/// of performance. Such a variable cannot be relied on to terminate iterative computation,
/// and it is important that any cycle of derivations have at least one de-duplicating
/// variable on it.
pub struct Variable<Tuple: Ord> {
    /// Should the variable be maintained distinctly.
    distinct: bool,
    /// A useful name for the variable.
    name: String,
    /// A list of relations whose union are the accepted tuples.
    pub stable: Rc<RefCell<Vec<Relation<Tuple>>>>,
    /// A list of recent tuples, still to be processed.
    pub recent: Rc<RefCell<Relation<Tuple>>>,
    /// A list of future tuples, to be introduced.
    to_add: Rc<RefCell<Vec<Relation<Tuple>>>>,
}

// Operator implementations.
impl<Tuple: Ord> Variable<Tuple> {
    /// Adds tuples that result from joining `input1` and `input2` --
    /// each of the inputs must be a set of (Key, Value) tuples. Both
    /// `input1` and `input2` must have the same type of key (`K`) but
    /// they can have distinct value types (`V1` and `V2`
    /// respectively). The `logic` closure will be invoked for each
    /// key that appears in both inputs; it is also given the two
    /// values, and from those it should construct the resulting
    /// value.
    ///
    /// Note that `input1` must be a variable, but `input2` can be a
    /// relation or a variable. Therefore, you cannot join two
    /// relations with this method. This is not because the result
    /// would be wrong, but because it would be inefficient: the
    /// result from such a join cannot vary across iterations (as
    /// relations are fixed), so you should prefer to invoke `insert`
    /// on a relation created by `Relation::from_join` instead.
    ///
    /// # Examples
    ///
    /// This example starts a collection with the pairs (x, x+1) and (x+1, x) for x in 0 .. 10.
    /// It then adds pairs (y, z) for which (x, y) and (x, z) are present. Because the initial
    /// pairs are symmetric, this should result in all pairs (x, y) for x and y in 0 .. 11.
    ///
    /// ```
    /// use datafrog::{Iteration, Relation};
    ///
    /// let mut iteration = Iteration::new();
    /// let variable = iteration.variable::<(usize, usize)>("source");
    /// variable.extend((0 .. 10).map(|x| (x, x + 1)));
    /// variable.extend((0 .. 10).map(|x| (x + 1, x)));
    ///
    /// while iteration.changed() {
    ///     variable.from_join(&variable, &variable, |&key, &val1, &val2| (val1, val2));
    /// }
    ///
    /// let result = variable.complete();
    /// assert_eq!(result.len(), 121);
    /// ```
    pub fn from_join<'me, K: Ord, V1: Ord, V2: Ord>(
        &self,
        input1: &'me Variable<(K, V1)>,
        input2: impl JoinInput<'me, (K, V2)>,
        logic: impl FnMut(&K, &V1, &V2) -> Tuple,
    ) {
        join::join_into(input1, input2, self, logic)
    }

    /// Adds tuples from `input1` whose key is not present in `input2`.
    ///
    /// Note that `input1` must be a variable: if you have a relation
    /// instead, you can use `Relation::from_antijoin` and then
    /// `Variable::insert`.  Note that the result will not vary during
    /// the iteration.
    ///
    /// # Examples
    ///
    /// This example starts a collection with the pairs (x, x+1) for x in 0 .. 10. It then
    /// adds any pairs (x+1,x) for which x is not a multiple of three. That excludes four
    /// pairs (for 0, 3, 6, and 9) which should leave us with 16 total pairs.
    ///
    /// ```
    /// use datafrog::{Iteration, Relation};
    ///
    /// let mut iteration = Iteration::new();
    /// let variable = iteration.variable::<(usize, usize)>("source");
    /// variable.extend((0 .. 10).map(|x| (x, x + 1)));
    ///
    /// let relation: Relation<_> = (0 .. 10).filter(|x| x % 3 == 0).collect();
    ///
    /// while iteration.changed() {
    ///     variable.from_antijoin(&variable, &relation, |&key, &val| (val, key));
    /// }
    ///
    /// let result = variable.complete();
    /// assert_eq!(result.len(), 16);
    /// ```
    pub fn from_antijoin<K: Ord, V: Ord>(
        &self,
        input1: &Variable<(K, V)>,
        input2: &Relation<K>,
        logic: impl FnMut(&K, &V) -> Tuple,
    ) {
        self.insert(join::antijoin(input1, input2, logic))
    }

    /// Adds tuples that result from mapping `input`.
    ///
    /// # Examples
    ///
    /// This example starts a collection with the pairs (x, x) for x in 0 .. 10. It then
    /// repeatedly adds any pairs (x, z) for (x, y) in the collection, where z is the Collatz
    /// step for y: it is y/2 if y is even, and 3*y + 1 if y is odd. This produces all of the
    /// pairs (x, y) where x visits y as part of its Collatz journey.
    ///
    /// ```
    /// use datafrog::{Iteration, Relation};
    ///
    /// let mut iteration = Iteration::new();
    /// let variable = iteration.variable::<(usize, usize)>("source");
    /// variable.extend((0 .. 10).map(|x| (x, x)));
    ///
    /// while iteration.changed() {
    ///     variable.from_map(&variable, |&(key, val)|
    ///         if val % 2 == 0 {
    ///             (key, val/2)
    ///         }
    ///         else {
    ///             (key, 3*val + 1)
    ///         });
    /// }
    ///
    /// let result = variable.complete();
    /// assert_eq!(result.len(), 74);
    /// ```
    pub fn from_map<T2: Ord>(&self, input: &Variable<T2>, logic: impl FnMut(&T2) -> Tuple) {
        map::map_into(input, self, logic)
    }

    /// Adds tuples that result from combining `source` with the
    /// relations given in `leapers`. This operation is very flexible
    /// and can be used to do a combination of joins and anti-joins.
    /// The main limitation is that the things being combined must
    /// consist of one dynamic variable (`source`) and then several
    /// fixed relations (`leapers`).
    ///
    /// The idea is as follows:
    ///
    /// - You will be inserting new tuples that result from joining (and anti-joining)
    ///   some dynamic variable `source` of source tuples (`SourceTuple`)
    ///   with some set of values (of type `Val`).
    /// - You provide these values by combining `source` with a set of leapers
    ///   `leapers`, each of which is derived from a fixed relation. The `leapers`
    ///   should be either a single leaper (of suitable type) or else a tuple of leapers.
    ///   You can create a leaper in one of two ways:
    ///   - Extension: In this case, you have a relation of type `(K, Val)` for some
    ///     type `K`. You provide a closure that maps from `SourceTuple` to the key
    ///     `K`. If you use `relation.extend_with`, then any `Val` values the
    ///     relation provides will be added to the set of values; if you use
    ///     `extend_anti`, then the `Val` values will be removed.
    ///   - Filtering: In this case, you have a relation of type `K` for some
    ///     type `K` and you provide a closure that maps from `SourceTuple` to
    ///     the key `K`. Filters don't provide values but they remove source
    ///     tuples.
    /// - Finally, you get a callback `logic` that accepts each `(SourceTuple, Val)`
    ///   that was successfully joined (and not filtered) and which maps to the
    ///   type of this variable.
    pub fn from_leapjoin<'leap, SourceTuple: Ord, Val: Ord + 'leap>(
        &self,
        source: &Variable<SourceTuple>,
        leapers: impl Leapers<'leap, SourceTuple, Val>,
        logic: impl FnMut(&SourceTuple, &Val) -> Tuple,
    ) {
        self.insert(treefrog::leapjoin(&source.recent.borrow(), leapers, logic));
    }
}

impl<Tuple: Ord> Clone for Variable<Tuple> {
    fn clone(&self) -> Self {
        Variable {
            distinct: self.distinct,
            name: self.name.clone(),
            stable: self.stable.clone(),
            recent: self.recent.clone(),
            to_add: self.to_add.clone(),
        }
    }
}

impl<Tuple: Ord> Variable<Tuple> {
    fn new(name: &str) -> Self {
        Variable {
            distinct: true,
            name: name.to_string(),
            stable: Rc::new(RefCell::new(Vec::new())),
            recent: Rc::new(RefCell::new(Vec::new().into())),
            to_add: Rc::new(RefCell::new(Vec::new())),
        }
    }

    /// Inserts a relation into the variable.
    ///
    /// This is most commonly used to load initial values into a variable.
    /// it is not obvious that it should be commonly used otherwise, but
    /// it should not be harmful.
    pub fn insert(&self, relation: Relation<Tuple>) {
        if !relation.is_empty() {
            self.to_add.borrow_mut().push(relation);
        }
    }

    /// Extend the variable with values from the iterator.
    ///
    /// This is most commonly used to load initial values into a variable.
    /// it is not obvious that it should be commonly used otherwise, but
    /// it should not be harmful.
    pub fn extend<T>(&self, iterator: impl IntoIterator<Item = T>)
    where
        Relation<Tuple>: FromIterator<T>,
    {
        self.insert(iterator.into_iter().collect());
    }

    /// Consumes the variable and returns a relation.
    ///
    /// This method removes the ability for the variable to develop, and
    /// flattens all internal tuples down to one relation. The method
    /// asserts that iteration has completed, in that `self.recent` and
    /// `self.to_add` should both be empty.
    pub fn complete(self) -> Relation<Tuple> {
        assert!(self.recent.borrow().is_empty());
        assert!(self.to_add.borrow().is_empty());
        let mut result: Relation<Tuple> = Vec::new().into();
        while let Some(batch) = self.stable.borrow_mut().pop() {
            result = result.merge(batch);
        }
        result
    }
}

impl<Tuple: Ord> VariableTrait for Variable<Tuple> {
    fn changed(&mut self) -> bool {
        // 1. Merge self.recent into self.stable.
        if !self.recent.borrow().is_empty() {
            let mut recent =
                ::std::mem::replace(&mut (*self.recent.borrow_mut()), Vec::new().into());
            while self
                .stable
                .borrow()
                .last()
                .map(|x| x.len() <= 2 * recent.len())
                == Some(true)
            {
                let last = self.stable.borrow_mut().pop().unwrap();
                recent = recent.merge(last);
            }
            self.stable.borrow_mut().push(recent);
        }

        // 2. Move self.to_add into self.recent.
        let to_add = self.to_add.borrow_mut().pop();
        if let Some(mut to_add) = to_add {
            while let Some(to_add_more) = self.to_add.borrow_mut().pop() {
                to_add = to_add.merge(to_add_more);
            }
            // 2b. Restrict `to_add` to tuples not in `self.stable`.
            if self.distinct {
                for batch in self.stable.borrow().iter() {
                    let mut slice = &batch[..];
                    // Only gallop if the slice is relatively large.
                    if slice.len() > 4 * to_add.elements.len() {
                        to_add.elements.retain(|x| {
                            slice = join::gallop(slice, |y| y < x);
                            slice.is_empty() || &slice[0] != x
                        });
                    } else {
                        to_add.elements.retain(|x| {
                            while !slice.is_empty() && &slice[0] < x {
                                slice = &slice[1..];
                            }
                            slice.is_empty() || &slice[0] != x
                        });
                    }
                }
            }
            *self.recent.borrow_mut() = to_add;
        }

        // let mut total = 0;
        // for tuple in self.stable.borrow().iter() {
        //     total += tuple.len();
        // }

        // println!("Variable\t{}\t{}\t{}", self.name, total, self.recent.borrow().len());

        !self.recent.borrow().is_empty()
    }
}

// impl<Tuple: Ord> Drop for Variable<Tuple> {
//     fn drop(&mut self) {
//         let mut total = 0;
//         for batch in self.stable.borrow().iter() {
//             total += batch.len();
//         }
//         println!("FINAL: {:?}\t{:?}", self.name, total);
//     }
// }
