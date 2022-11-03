#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

#[lang = "clone"]
pub trait Clone: Sized {
    fn clone(&self) -> Self;

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}

mod impls {
    use super::Clone;

    macro_rules! impl_clone {
        ($($t:ty)*) => {
            $(
                impl Clone for $t {
                    fn clone(&self) -> Self {
                        *self
                    }
                }
            )*
        }
    }

    impl_clone! {
        usize u8 u16 u32 u64 // u128
        isize i8 i16 i32 i64 // i128
        f32 f64
        bool char
    }
}

#[lang = "copy"]
pub trait Copy: Clone {
    // Empty.
}

mod copy_impls {
    use super::Copy;

    macro_rules! impl_copy {
        ($($t:ty)*) => {
            $(
                impl Copy for $t {}
            )*
        }
    }

    impl_copy! {
        usize u8 u16 u32 u64 // u128
        isize i8 i16 i32 i64 // i128
        f32 f64
        bool char
    }
}

extern "rust-intrinsic" {
    pub fn atomic_store_seqcst<T: Copy>(dst: *mut T, val: T);
    pub fn atomic_store_release<T: Copy>(dst: *mut T, val: T);
    pub fn atomic_store_relaxed<T: Copy>(dst: *mut T, val: T);
    pub fn atomic_store_unordered<T: Copy>(dst: *mut T, val: T);
}

fn main() {
    let mut dst = 15u32;
    let new_value = 14;

    unsafe {
        atomic_store_seqcst(&mut dst, new_value);
        atomic_store_release(&mut dst, new_value);
        atomic_store_relaxed(&mut dst, new_value);
        atomic_store_unordered(&mut dst, new_value);
    }
}
