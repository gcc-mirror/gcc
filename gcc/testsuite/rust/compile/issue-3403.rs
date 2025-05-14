pub struct Foo {
    a: i32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}
pub struct Bar(i32);

#[lang = "sized"]
trait Sized {}

pub mod core {
    pub mod default {
        pub trait Default: Sized {
            fn default() -> Self;
        }

        impl Default for i32 {
            fn default() -> Self {
                0
            }
        }
    }
}

impl ::core::default::Default for Bar {
    #[inline]
    fn default() -> Bar {
        Bar(core::default::Default::default())
    }
}

impl ::core::default::Default for Foo {
    #[inline]
    fn default() -> Foo {
        Foo {
            a: core::default::Default::default(),
        }
    }
}
