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
    pub fn atomic_store_seqcst<T: Copy>(dst: *mut T, value: T);
    // { dg-error "atomic intrinsics can only be used with basic integer types .got .VeryLargeType.." "" { target *-*-* } .-1 }
    // { dg-error "atomic intrinsics can only be used with basic integer types .got .bool.." "" { target *-*-* } .-2 }
}

struct VeryLargeType {
    a0: i128,
    a1: i128,
    a2: i128,
    a3: i128,
}

impl VeryLargeType {
    pub fn new(value: i128) -> VeryLargeType {
        VeryLargeType {
            a0: value,
            a1: 0,
            a2: 0,
            a3: 0,
        }
    }
}

impl Clone for VeryLargeType {
    fn clone(&self) -> Self {
        *self
    }
}
impl Copy for VeryLargeType {}

fn main() {
    let mut dst = VeryLargeType::new(0);
    let mut b = false;

    unsafe {
        atomic_store_seqcst(&mut dst, VeryLargeType::new(1));
        atomic_store_seqcst(&mut b, true);
    }
}
