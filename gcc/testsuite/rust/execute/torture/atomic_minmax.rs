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
    pub fn atomic_min<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_min_acq<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_min_rel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_min_acqrel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_min_relaxed<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_max<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_max_acq<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_max_rel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_max_acqrel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_max_relaxed<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umin<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umin_acq<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umin_rel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umin_acqrel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umin_relaxed<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umax<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umax_acq<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umax_rel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umax_acqrel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_umax_relaxed<T: Copy>(dst: *mut T, val: T) -> T;
}

fn main() -> u32 {
    unsafe {
        let mut src = 9i8;

        let old = atomic_min(&mut src, -7);
        if old != 9 || src != -7 {
            return 1;
        }

        let old = atomic_min_acq(&mut src, 9);
        if old != -7 || src != -7 {
            return 1;
        }

        let old = atomic_min_rel(&mut src, -9);
        if old != -7 || src != -9 {
            return 1;
        }

        let old = atomic_min_acqrel(&mut src, -9);
        if old != -9 || src != -9 {
            return 1;
        }

        let old = atomic_min_relaxed(&mut src, -10);
        if old != -9 || src != -10 {
            return 1;
        }

        let mut src = -7i8;

        let old = atomic_max(&mut src, 9);
        if old != -7 || src != 9 {
            return 1;
        }

        let old = atomic_max_acq(&mut src, -7);
        if old != 9 || src != 9 {
            return 1;
        }

        let old = atomic_max_rel(&mut src, 10);
        if old != 9 || src != 10 {
            return 1;
        }

        let old = atomic_max_acqrel(&mut src, 10);
        if old != 10 || src != 10 {
            return 1;
        }

        let old = atomic_max_relaxed(&mut src, 11);
        if old != 10 || src != 11 {
            return 1;
        }

        let mut src = 249u8;

        let old = atomic_umin(&mut src, 9);
        if old != 249 || src != 9 {
            return 1;
        }

        let old = atomic_umin_acq(&mut src, 249);
        if old != 9 || src != 9 {
            return 1;
        }

        let old = atomic_umin_rel(&mut src, 7);
        if old != 9 || src != 7 {
            return 1;
        }

        let old = atomic_umin_acqrel(&mut src, 7);
        if old != 7 || src != 7 {
            return 1;
        }

        let old = atomic_umin_relaxed(&mut src, 6);
        if old != 7 || src != 6 {
            return 1;
        }

        let mut src = 9u8;

        let old = atomic_umax(&mut src, 249);
        if old != 9 || src != 249 {
            return 1;
        }

        let old = atomic_umax_acq(&mut src, 9);
        if old != 249 || src != 249 {
            return 1;
        }

        let old = atomic_umax_rel(&mut src, 250);
        if old != 249 || src != 250 {
            return 1;
        }

        let old = atomic_umax_acqrel(&mut src, 250);
        if old != 250 || src != 250 {
            return 1;
        }

        let old = atomic_umax_relaxed(&mut src, 251);
        if old != 250 || src != 251 {
            return 1;
        }

        // funny non-matching signedness tests below
        let mut src = 128u8;

        let old = atomic_min(&mut src, 7);
        if old != 128 || src != 128 {
            return 1;
        }

        let old = atomic_max(&mut src, 7);
        if old != 128 || src != 7 {
            return 1;
        }

        let mut src = 7i8;

        let old = atomic_umax(&mut src, -128);
        if old != 7 || src != -128 {
            return 1;
        }

        let old = atomic_umin(&mut src, 7);
        if old != -128 || src != 7 {
            return 1;
        }
    }

    0
}
