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
    pub fn atomic_xor<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_xor_acq<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_xor_rel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_xor_acqrel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_xor_relaxed<T: Copy>(dst: *mut T, val: T) -> T;
}

fn main() -> u32 {
    unsafe {
        let mut src = 0b10101u32;

        let one = atomic_xor(&mut src, 0b00111);
        if one != 0b10101 || src != 0b10010 {
            return 1;
        }

        let two = atomic_xor_acq(&mut src, 0b11000);
        if two != 0b10010 || src != 0b01010 {
            return 1;
        }

        let three = atomic_xor_rel(&mut src, 0b01111);
        if three != 0b01010 || src != 0b00101 {
            return 1;
        }

        let four = atomic_xor_acqrel(&mut src, 0b10101);
        if four != 0b00101 || src != 0b10000 {
            return 1;
        }

        let five = atomic_xor_relaxed(&mut src, 0b10000);
        if five != 0b10000 || src != 0 {
            return 1;
        }
    }

    0
}
