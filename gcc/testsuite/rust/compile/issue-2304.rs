#[lang = "sized"]
pub trait Sized {}

#[lang = "add"]
pub trait Add<RHS = Self> {
    type Output;

    fn add(self, rhs: RHS) -> Self::Output;
}
macro_rules! add_impl {
    ($($t:ty)*) => ($(
        impl Add for $t {
            type Output = $t;

            fn add(self, other: $t) -> $t { self + other }
        }
    )*)
}

add_impl! { usize u8 u16 u32 u64  /*isize i8 i16 i32 i64*/  f32 f64 }

pub fn test() {
    let x: usize = 123;
    let mut i = 0;
    let _bug = i + x;
}
