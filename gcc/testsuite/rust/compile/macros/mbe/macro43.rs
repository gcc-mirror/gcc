use Option::{None, Some};

enum Option<T> {
    None,
    Some(T)
}

macro_rules! nonzero_integers {
    ( $( $Ty: ident($Int: ty); )+ ) => {
        $(
            /// An integer that is known not to equal zero.
            ///
            /// This enables some memory layout optimization.
            /// For example, `Option<NonZeroU32>` is the same size as `u32`:
            ///
            /// ```rust
            /// use std::mem::size_of;
            /// assert_eq!(size_of::<Option<std::num::NonZeroU32>>(), size_of::<u32>());
            /// ```
            #[stable(feature = "nonzero", since = "1.28.0")]
            // not all derive macros are implemented yet, and this test does not test these anyways
            // #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
            #[repr(transparent)]
            pub struct $Ty($Int);

            impl $Ty {
                /// Create a non-zero without checking the value.
                ///
                /// # Safety
                ///
                /// The value must not be zero.
                #[stable(feature = "nonzero", since = "1.28.0")]
                #[inline]
                pub const unsafe fn new_unchecked(n: $Int) -> Self {
                    $Ty(n)
                }

                /// Create a non-zero if the given value is not zero.
                #[stable(feature = "nonzero", since = "1.28.0")]
                #[inline]
                pub fn new(n: $Int) -> Option<Self> {
                    if n != 0 {
                        Some($Ty(n))
                    } else {
                        None
                    }
                }

                /// Returns the value as a primitive type.
                #[stable(feature = "nonzero", since = "1.28.0")]
                #[inline]
                pub fn get(self) -> $Int {
                    self.0
                }

            }

            impl_nonzero_fmt! { // { dg-error "could not resolve macro invocation" }
                (Debug, Display, Binary, Octal, LowerHex, UpperHex) for $Ty
            }
        )+
    }
}

nonzero_integers! {
    NonZeroU8(u8);
    NonZeroU16(u16);
    NonZeroU32(u32);
    NonZeroU64(u64);
    NonZeroU128(u128);
    NonZeroUsize(usize);
}
