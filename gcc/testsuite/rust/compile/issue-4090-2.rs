mod core {
    mod marker {
        #[lang = "sized"]
        pub trait Sized {}

        #[lang = "phantom_data"]
        #[stable(feature = "rust1", since = "1.0.0")]
        pub struct PhantomData<T: ?Sized>;

        #[unstable(feature = "structural_match", issue = "31434")]
        #[lang = "structural_teq"]
        pub trait StructuralEq {
            // Empty.
        }

        #[unstable(feature = "structural_match", issue = "31434")]
        #[lang = "structural_peq"]
        pub trait StructuralPartialEq {
            // Empty.
        }
    }

    pub mod cmp {
        use super::marker::Sized;

        #[lang = "eq"]
        pub trait PartialEq<Rhs: ?Sized = Self> {
            fn eq(&self, other: &Rhs) -> bool;

            fn ne(&self, other: &Rhs) -> bool {
                !self.eq(other)
            }
        }

        pub trait Eq: PartialEq<Self> {
            fn assert_receiver_is_total_eq(&self) {}
        }
    }

    pub mod ptr {

        use super::cmp::{Eq, PartialEq};

        macro_rules! fnptr_impls_safety_abi {
            ($FnTy: ty, $($Arg: ident),*) => {
                #[stable(feature = "fnptr_impls", since = "1.4.0")]
                impl<Ret, $($Arg),*> PartialEq for $FnTy {
                    #[inline]
                    fn eq(&self, other: &Self) -> bool {
                        *self as usize == *other as usize
                    }
                }

                #[stable(feature = "fnptr_impls", since = "1.4.0")]
                impl<Ret, $($Arg),*> Eq for $FnTy {}

            }
        }

        fnptr_impls_safety_abi! { extern "Rust" fn() -> Ret, }
        fnptr_impls_safety_abi! { extern "C" fn() -> Ret, }
        fnptr_impls_safety_abi! { unsafe extern "Rust" fn() -> Ret, }
        fnptr_impls_safety_abi! { unsafe extern "C" fn() -> Ret, }
    }
}

#[derive(PartialEq, Eq)]
struct AllowedBelow {
    // { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
    f: fn(),
}
