#![feature(rustc_attrs)]

#[lang = "sized"]
trait Sized {}

#[lang = "add"]
trait Add<Rhs = Self> {
    type Output;

    fn add(self, rhs: Rhs) -> Self::Output;
}

macro_rules! add_impl {
    ($($t:ty)*) => ($(
        impl Add for $t {
            type Output = $t;

            #[inline]
            #[rustc_inherit_overflow_checks]
            fn add(self, other: $t) -> $t { self + other }
        }
    )*)
}

add_impl! { usize u8 u16 u32 u64 u128 isize i8 i16 i32 i64 i128 f32 f64 }

pub fn test(len: usize) -> u64 {
    let mut i = 0;
    let mut out = 0;
    if i + 3 < len {
        out = 123;
    } else {
        out = 456;
    }
    out
}
