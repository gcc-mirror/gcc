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
    pub fn atomic_xsub<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_xsub_acq<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_xsub_rel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_xsub_acqrel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_xsub_relaxed<T: Copy>(dst: *mut T, val: T) -> T;
}

fn main() -> u32 {
    let one;
    let two;
    let three;
    let four;
    let five;
    let mut src = 5u32;

    unsafe {
        five = atomic_xsub(&mut src, 1);
        four = atomic_xsub_acq(&mut src, 1);
        three = atomic_xsub_rel(&mut src, 1);
        two = atomic_xsub_acqrel(&mut src, 1);
        one = atomic_xsub_relaxed(&mut src, 1);

        if one != 1 || two != 2 || three != 3 || four != 4 || five != 5 {
            return 1;
        }
    }

    src
}
