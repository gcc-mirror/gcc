#![feature(no_core)]
#![no_core]

#![feature(intrinsics)]

#![feature(lang_items)]
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
    fn atomic_min<T>(dst: *mut T, val: T) -> T;
    fn atomic_umax<T>(dst: *mut T, val: T) -> T;
}

fn main() {
    let mut flag = true;
    let mut value = 1.0f32;
    unsafe {
        atomic_min(&mut flag, false); // { dg-error "atomic operation intrinsic can only be used with basic integer types .got .bool.." }
        atomic_umax(&mut value, 2.0f32); // { dg-error "atomic operation intrinsic can only be used with basic integer types .got .f32.." }
    }
}
