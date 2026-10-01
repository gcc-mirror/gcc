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
    pub fn atomic_and<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_and_acq<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_and_rel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_and_acqrel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_and_relaxed<T: Copy>(dst: *mut T, val: T) -> T;
}

fn main() -> u32 {
    unsafe {
        let mut src = 0b11111u32;

        let one = atomic_and(&mut src, 0b11110);
        if one != 0b11111 || src != 0b11110 {
            return 1;
        }

        let two = atomic_and_acq(&mut src, 0b11101);
        if two != 0b11110 || src != 0b11100 {
            return 1;
        }

        let three = atomic_and_rel(&mut src, 0b11011);
        if three != 0b11100 || src != 0b11000 {
            return 1;
        }

        let four = atomic_and_acqrel(&mut src, 0b10111);
        if four != 0b11000 || src != 0b10000 {
            return 1;
        }

        let five = atomic_and_relaxed(&mut src, 0b01111);
        if five != 0b10000 || src != 0 {
            return 1;
        }
    }

    0
}
