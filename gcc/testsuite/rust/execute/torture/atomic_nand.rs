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
    pub fn atomic_nand<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_nand_acq<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_nand_rel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_nand_acqrel<T: Copy>(dst: *mut T, val: T) -> T;
    pub fn atomic_nand_relaxed<T: Copy>(dst: *mut T, val: T) -> T;
}

fn main() -> u32 {
    unsafe {
        let mut src = 0b11110000u8;

        let one = atomic_nand(&mut src, 0b10101010);
        if one != 0b11110000 || src != 0b01011111 {
            return 1;
        }

        let two = atomic_nand_acq(&mut src, 0b11001100);
        if two != 0b01011111 || src != 0b10110011 {
            return 1;
        }

        let three = atomic_nand_rel(&mut src, 0b11110000);
        if three != 0b10110011 || src != 0b01001111 {
            return 1;
        }

        let four = atomic_nand_acqrel(&mut src, 0b00001111);
        if four != 0b01001111 || src != 0b11110000 {
            return 1;
        }

        let five = atomic_nand_relaxed(&mut src, 0b11111111);
        if five != 0b11110000 || src != 0b00001111 {
            return 1;
        }
    }

    0
}
