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
    pub fn atomic_load_seqcst<T: Copy>(src: *const T) -> T;
    pub fn atomic_load_acquire<T: Copy>(src: *const T) -> T;
    pub fn atomic_load_relaxed<T: Copy>(src: *const T) -> T;
    pub fn atomic_load_unordered<T: Copy>(src: *const T) -> T;
}

fn main() -> i32 {
    let one;
    let two;
    let three;
    let four;

    unsafe {
        let mut src = 1;
        one = atomic_load_seqcst(&src);

        src = 2;
        two = atomic_load_acquire(&src);

        src = 3;
        three = atomic_load_relaxed(&src);

        src = 4;
        four = atomic_load_unordered(&src);
    }

    (four + three + two + one) - 10
}
