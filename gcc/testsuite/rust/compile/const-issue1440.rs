// { dg-additional-options "-w" }
#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn wrapping_add<T>(a: T, b: T) -> T;
        pub fn rotate_left<T>(a: T, b: T) -> T;
        pub fn rotate_right<T>(a: T, b: T) -> T;
        pub fn offset<T>(ptr: *const T, count: isize) -> *const T;
    }
}

mod mem {
    extern "rust-intrinsic" {
        #[rustc_const_stable(feature = "const_transmute", since = "1.46.0")]
        pub fn transmute<T, U>(_: T) -> U;
        pub fn size_of<T>() -> usize;
    }
}

macro_rules! impl_uint {
    ($($ty:ident = $lang:literal),*) => {
        $(
            impl $ty {
                pub fn wrapping_add(self, rhs: Self) -> Self {
                    // intrinsics::wrapping_add(self, rhs)
                    self + rhs
                }

                pub fn rotate_left(self, n: u32) -> Self {
                    unsafe {
                        intrinsics::rotate_left(self, n as Self)
                    }
                }

                pub fn rotate_right(self, n: u32) -> Self {
                    unsafe {
                        intrinsics::rotate_right(self, n as Self)
                    }
                }

                pub fn to_le(self) -> Self {
                    {
                        self
                    }
                }

                pub const fn from_le_bytes(bytes: [u8; mem::size_of::<Self>()]) -> Self {
                    // { dg-error "only functions marked as .const. are allowed to be called from constant contexts" "" { target *-*-* } .-1 }
                    Self::from_le(Self::from_ne_bytes(bytes))
                }

                pub const fn from_le(x: Self) -> Self {
                    #[cfg(target_endian = "little")]
                    {
                        x
                    }
                }

                pub const fn from_ne_bytes(bytes: [u8; mem::size_of::<Self>()]) -> Self {
                    // { dg-error "only functions marked as .const. are allowed to be called from constant contexts" "" { target *-*-* } .-1 }
                    unsafe { mem::transmute(bytes) }
                }
            }
        )*
    }
}

impl_uint!(
    u8 = "u8",
    u16 = "u16",
    u32 = "u32",
    u64 = "u64",
    u128 = "u128",
    usize = "usize"
);
