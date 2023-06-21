mod core {
    mod ops {
        #[lang = "add"]
        pub trait Add<Rhs = Self> {
            type Output;

            fn add(self, rhs: Rhs) -> Self::Output;
        }
    }
}

impl core::ops::Add for f32 {
    type Output = f32;

    fn add(self, rhs: Self) -> Self::Output {
        self + rhs
    }
}

pub fn foo<T: core::ops::Add<Output = i32>>(a: T) -> i32 {
    a + a
}

pub fn main() {
    foo(123f32);
    // { dg-error "expected .i32. got .f32." "" { target *-*-* } .-1 }
    // { dg-error "bounds not satisfied for f32 .Add. is not satisfied" "" { target *-*-* } .-2 }
}
