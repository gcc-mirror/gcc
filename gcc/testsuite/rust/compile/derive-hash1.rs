#![feature(intrinsics)]

#[lang = "sized"]
trait Sized {}

pub mod core {
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

    pub mod hash {
        pub trait Hasher {}

        pub trait Hash {
            /// Feeds this value into the given [`Hasher`].
            ///
            /// # Examples
            ///
            /// ```
            /// use std::collections::hash_map::DefaultHasher;
            /// use std::hash::{Hash, Hasher};
            ///
            /// let mut hasher = DefaultHasher::new();
            /// 7920.hash(&mut hasher);
            /// println!("Hash is {:x}!", hasher.finish());
            /// ```
            #[stable(feature = "rust1", since = "1.0.0")]
            fn hash<H: Hasher>(&self, state: &mut H);

            /// Feeds a slice of this type into the given [`Hasher`].
            ///
            /// # Examples
            ///
            /// ```
            /// use std::collections::hash_map::DefaultHasher;
            /// use std::hash::{Hash, Hasher};
            ///
            /// let mut hasher = DefaultHasher::new();
            /// let numbers = [6, 28, 496, 8128];
            /// Hash::hash_slice(&numbers, &mut hasher);
            /// println!("Hash is {:x}!", hasher.finish());
            /// ```
            #[stable(feature = "hash_slice", since = "1.3.0")]
            fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
            where
                Self: Sized,
            {
                // for piece in data {
                //     piece.hash(state);
                // }
            }
        }
    }
}

impl core::hash::Hash for i32 {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {}
}

impl core::hash::Hash for i64 {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {}
}

// for the discriminant value
impl core::hash::Hash for isize {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {}
}

#[derive(Hash)]
struct Foo { // { dg-warning "never constructed" }
    a: i32,
    b: i32,
}

#[derive(Hash)]
struct Bar(i32, i64); // { dg-warning "never constructed" }

#[derive(Hash)]
enum Baz {
    A,
    B(i32),
    C { a: i64 }
}
