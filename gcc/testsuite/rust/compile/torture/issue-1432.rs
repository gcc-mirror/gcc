// { dg-additional-options "-w" }
#![feature(intrinsics)]
mod intrinsics {
    extern "rust-intrinsic" {
        #[rustc_const_stable(feature = "const_int_wrapping", since = "1.40.0")]
        pub fn wrapping_add<T>(a: T, b: T) -> T;
        #[rustc_const_stable(feature = "const_int_rotate", since = "1.40.0")]
        pub fn rotate_left<T>(a: T, b: T) -> T;
        #[rustc_const_stable(feature = "const_int_rotate", since = "1.40.0")]
        pub fn rotate_right<T>(a: T, b: T) -> T;
        #[rustc_const_stable(feature = "const_ptr_offset", since = "1.61.0")]
        pub fn offset<T>(ptr: *const T, count: isize) -> *const T;
    }
}

mod mem {
    extern "rust-intrinsic" {
        #[rustc_const_stable(feature = "const_transmute", since = "1.46.0")]
        fn transmute<T, U>(_: T) -> U;
        #[rustc_const_stable(feature = "const_size_of", since = "1.40.0")]
        fn size_of<T>() -> usize;
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
                    #[cfg(target_endian = "little")]
                    {
                        self
                    }
                }

                pub const fn from_le_bytes(bytes: [u8; mem::size_of::<Self>()]) -> Self {
                    Self::from_le(Self::from_ne_bytes(bytes))
                }

                pub const fn from_le(x: Self) -> Self {
                    #[cfg(target_endian = "little")]
                    {
                        x
                    }
                }

                pub const fn from_ne_bytes(bytes: [u8; mem::size_of::<Self>()]) -> Self {
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
    usize = "usize"
);
