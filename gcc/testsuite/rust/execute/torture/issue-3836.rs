// { dg-options "-w" }
// { dg-output "less\r*\n" }

#![feature(intrinsics)]

mod core {
    mod option {
        // #[rustc_diagnostic_item = "option_type"]
        #[stable(feature = "rust1", since = "1.0.0")]
        pub enum Option<T> {
            /// No value
            #[lang = "None"]
            #[stable(feature = "rust1", since = "1.0.0")]
            None,
            /// Some value `T`
            #[lang = "Some"]
            #[stable(feature = "rust1", since = "1.0.0")]
            Some(#[stable(feature = "rust1", since = "1.0.0")] T),
        }
    }

    mod marker {
        #[lang = "phantom_data"]
        #[stable(feature = "rust1", since = "1.0.0")]
        pub struct PhantomData<T: ?Sized>;

        #[unstable(feature = "structural_match", issue = "31434")]
        // #[rustc_on_unimplemented(message = "the type `{Self}` does not `#[derive(PartialEq)]`")]
        #[lang = "structural_peq"]
        pub trait StructuralPartialEq {
            // Empty.
        }

        #[unstable(feature = "structural_match", issue = "31434")]
        // #[rustc_on_unimplemented(message = "the type `{Self}` does not `#[derive(Eq)]`")]
        #[lang = "structural_teq"]
        pub trait StructuralEq {
            // Empty.
        }

        #[stable(feature = "rust1", since = "1.0.0")]
        #[lang = "sized"]
        // #[rustc_on_unimplemented(
        //     message = "the size for values of type `{Self}` cannot be known at compilation time",
        //     label = "doesn't have a size known at compile-time"
        // )]
        // #[fundamental] // for Default, for example, which requires that `[T]: !Default` be evaluatable
        // #[rustc_specialization_trait]
        pub trait Sized {
            // Empty.
        }
    }

    mod cmp {
        use super::marker::Sized;
        use super::option::Option;

        // #[derive(Clone, Copy, PartialEq, Debug, Hash)]
        #[stable(feature = "rust1", since = "1.0.0")]
        pub enum Ordering {
            /// An ordering where a compared value is less than another.
            #[stable(feature = "rust1", since = "1.0.0")]
            Less = -1,
            /// An ordering where a compared value is equal to another.
            #[stable(feature = "rust1", since = "1.0.0")]
            Equal = 0,
            /// An ordering where a compared value is greater than another.
            #[stable(feature = "rust1", since = "1.0.0")]
            Greater = 1,
        }

        #[lang = "eq"]
        #[stable(feature = "rust1", since = "1.0.0")]
        #[doc(alias = "==")]
        #[doc(alias = "!=")]
        // #[rustc_on_unimplemented(
        //     message = "can't compare `{Self}` with `{Rhs}`",
        //     label = "no implementation for `{Self} == {Rhs}`"
        // )]
        pub trait PartialEq<Rhs: ?Sized = Self> {
            /// This method tests for `self` and `other` values to be equal, and is used
            /// by `==`.
            #[must_use]
            #[stable(feature = "rust1", since = "1.0.0")]
            fn eq(&self, other: &Rhs) -> bool;

            fn ne(&self, other: &Rhs) -> bool {
                !self.eq(other)
            }
        }

        #[doc(alias = "==")]
        #[doc(alias = "!=")]
        #[stable(feature = "rust1", since = "1.0.0")]
        pub trait Eq: PartialEq<Self> {
            // this method is used solely by #[deriving] to assert
            // that every component of a type implements #[deriving]
            // itself, the current deriving infrastructure means doing this
            // assertion without using a method on this trait is nearly
            // impossible.
            //
            // This should never be implemented by hand.
            #[doc(hidden)]
            #[stable(feature = "rust1", since = "1.0.0")]
            fn assert_receiver_is_total_eq(&self) {}
        }

        #[lang = "partial_ord"]
        #[stable(feature = "rust1", since = "1.0.0")]
        #[doc(alias = ">")]
        #[doc(alias = "<")]
        #[doc(alias = "<=")]
        #[doc(alias = ">=")]
        // #[rustc_on_unimplemented(
        //     message = "can't compare `{Self}` with `{Rhs}`",
        //     label = "no implementation for `{Self} < {Rhs}` and `{Self} > {Rhs}`"
        // )]
        pub trait PartialOrd<Rhs: ?Sized = Self>: PartialEq<Rhs> {
            /// This method returns an ordering between `self` and `other` values if one exists.
            ///
            /// # Examples
            ///
            /// ```
            /// use std::cmp::Ordering;
            ///
            /// let result = 1.0.partial_cmp(&2.0);
            /// assert_eq!(result, Some(Ordering::Less));
            ///
            /// let result = 1.0.partial_cmp(&1.0);
            /// assert_eq!(result, Some(Ordering::Equal));
            ///
            /// let result = 2.0.partial_cmp(&1.0);
            /// assert_eq!(result, Some(Ordering::Greater));
            /// ```
            ///
            /// When comparison is impossible:
            ///
            /// ```
            /// let result = f64::NAN.partial_cmp(&1.0);
            /// assert_eq!(result, None);
            /// ```
            #[must_use]
            #[stable(feature = "rust1", since = "1.0.0")]
            fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;

            /// This method tests less than (for `self` and `other`) and is used by the `<` operator.
            ///
            /// # Examples
            ///
            /// ```
            /// let result = 1.0 < 2.0;
            /// assert_eq!(result, true);
            ///
            /// let result = 2.0 < 1.0;
            /// assert_eq!(result, false);
            /// ```
            #[inline]
            #[must_use]
            #[stable(feature = "rust1", since = "1.0.0")]
            fn lt(&self, other: &Rhs) -> bool {
                match self.partial_cmp(other) {
                    Option::Some(Ordering::Less) => true,
                    _ => false,
                }
            }

            /// This method tests less than or equal to (for `self` and `other`) and is used by the `<=`
            /// operator.
            ///
            /// # Examples
            ///
            /// ```
            /// let result = 1.0 <= 2.0;
            /// assert_eq!(result, true);
            ///
            /// let result = 2.0 <= 2.0;
            /// assert_eq!(result, true);
            /// ```
            #[inline]
            #[must_use]
            #[stable(feature = "rust1", since = "1.0.0")]
            fn le(&self, other: &Rhs) -> bool {
                match self.partial_cmp(other) {
                    Option::Some(Ordering::Less | Ordering::Equal) => true,
                    _ => false,
                }
            }

            /// This method tests greater than (for `self` and `other`) and is used by the `>` operator.
            ///
            /// # Examples
            ///
            /// ```
            /// let result = 1.0 > 2.0;
            /// assert_eq!(result, false);
            ///
            /// let result = 2.0 > 2.0;
            /// assert_eq!(result, false);
            /// ```
            #[inline]
            #[must_use]
            #[stable(feature = "rust1", since = "1.0.0")]
            fn gt(&self, other: &Rhs) -> bool {
                match self.partial_cmp(other) {
                    Option::Some(Ordering::Greater) => true,
                    _ => false,
                }
            }

            /// This method tests greater than or equal to (for `self` and `other`) and is used by the `>=`
            /// operator.
            ///
            /// # Examples
            ///
            /// ```
            /// let result = 2.0 >= 1.0;
            /// assert_eq!(result, true);
            ///
            /// let result = 2.0 >= 2.0;
            /// assert_eq!(result, true);
            /// ```
            #[inline]
            #[must_use]
            #[stable(feature = "rust1", since = "1.0.0")]
            fn ge(&self, other: &Rhs) -> bool {
                match self.partial_cmp(other) {
                    Option::Some(Ordering::Greater | Ordering::Equal) => true,
                    _ => false,
                }
            }
        }

        #[doc(alias = "<")]
        #[doc(alias = ">")]
        #[doc(alias = "<=")]
        #[doc(alias = ">=")]
        #[stable(feature = "rust1", since = "1.0.0")]
        pub trait Ord: Eq + PartialOrd<Self> {
            /// This method returns an [`Ordering`] between `self` and `other`.
            ///
            /// By convention, `self.cmp(&other)` returns the ordering matching the expression
            /// `self <operator> other` if true.
            ///
            /// # Examples
            ///
            /// ```
            /// use std::cmp::Ordering;
            ///
            /// assert_eq!(5.cmp(&10), Ordering::Less);
            /// assert_eq!(10.cmp(&5), Ordering::Greater);
            /// assert_eq!(5.cmp(&5), Ordering::Equal);
            /// ```
            #[must_use]
            #[stable(feature = "rust1", since = "1.0.0")]
            fn cmp(&self, other: &Self) -> Ordering;

            /// Compares and returns the maximum of two values.
            ///
            /// Returns the second argument if the comparison determines them to be equal.
            ///
            /// # Examples
            ///
            /// ```
            /// assert_eq!(2, 1.max(2));
            /// assert_eq!(2, 2.max(2));
            /// ```
            #[stable(feature = "ord_max_min", since = "1.21.0")]
            #[must_use]
            fn max(self, other: Self) -> Self
            where
                Self: Sized,
            {
                self
            }

            /// Compares and returns the minimum of two values.
            ///
            /// Returns the first argument if the comparison determines them to be equal.
            ///
            /// # Examples
            ///
            /// ```
            /// assert_eq!(1, 1.min(2));
            /// assert_eq!(2, 2.min(2));
            /// ```
            #[stable(feature = "ord_max_min", since = "1.21.0")]
            #[must_use]
            fn min(self, other: Self) -> Self
            where
                Self: Sized,
            {
                self
            }

            /// Restrict a value to a certain interval.
            ///
            /// Returns `max` if `self` is greater than `max`, and `min` if `self` is
            /// less than `min`. Otherwise this returns `self`.
            ///
            /// # Panics
            ///
            /// Panics if `min > max`.
            ///
            /// # Examples
            ///
            /// ```
            /// #![feature(clamp)]
            ///
            /// assert!((-3).clamp(-2, 1) == -2);
            /// assert!(0.clamp(-2, 1) == 0);
            /// assert!(2.clamp(-2, 1) == 1);
            /// ```
            #[must_use]
            #[unstable(feature = "clamp", issue = "44095")]
            fn clamp(self, min: Self, max: Self) -> Self
            where
                Self: Sized,
            {
                if self < min {
                    min
                } else if self > max {
                    max
                } else {
                    self
                }
            }
        }
    }

    pub mod intrinsics {
        #[lang = "discriminant_kind"]
        pub trait DiscriminantKind {
            #[lang = "discriminant_type"]
            type Discriminant;
        }

        extern "rust-intrinsic" {
            pub fn discriminant_value<T>(v: &T) -> <T as DiscriminantKind>::Discriminant;
        }
    }
}

use core::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use core::marker::Sized;
use core::option::Option;

// for comparing discriminant_value
impl PartialEq for isize {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

// for comparing discriminant_value
impl PartialOrd for isize {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if *self > *other {
            Option::Some(Ordering::Greater)
        } else if *self < *other {
            Option::Some(Ordering::Less)
        } else {
            Option::Some(Ordering::Equal)
        }
    }

    fn lt(&self, other: &Self) -> bool {
        *self < *other
    }
    fn le(&self, other: &Self) -> bool {
        *self <= *other
    }
    fn ge(&self, other: &Self) -> bool {
        *self >= *other
    }
    fn gt(&self, other: &Self) -> bool {
        *self > *other
    }
}

impl PartialEq for i32 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}
impl Eq for i32 {}

impl PartialOrd for i32 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if *self > *other {
            Option::Some(Ordering::Greater)
        } else if *self < *other {
            Option::Some(Ordering::Less)
        } else {
            Option::Some(Ordering::Equal)
        }
    }

    fn lt(&self, other: &Self) -> bool {
        *self < *other
    }
    fn le(&self, other: &Self) -> bool {
        *self <= *other
    }
    fn ge(&self, other: &Self) -> bool {
        *self >= *other
    }
    fn gt(&self, other: &Self) -> bool {
        *self > *other
    }
}

impl Ord for i32 {
    fn cmp(&self, other: &Self) -> Ordering {
        if *self > *other {
            Ordering::Greater
        } else if *self < *other {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    }
}

// ------------

#[derive(Ord, PartialOrd, PartialEq, Eq)]
struct Bar {
    a: i32,
    b: i32,
}

extern "C" {
    fn puts(s: *const i8);
}

fn print(s: &str) {
    unsafe {
        puts(s as *const str as *const i8);
    }
}

fn main() -> i32 {
    let x = Bar { a: 1, b: 2 };
    let y = Bar { a: 1, b: 3 };

    match x.partial_cmp(&y) {
        Option::Some(Ordering::Less) => print("less"),
        Option::Some(Ordering::Greater) => print("greater"),
        Option::Some(Ordering::Equal) => print("equal"),
        _ => print("none"),
    }

    0
}
