#[lang = "sized"]
pub trait Sized {}

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn uninit<T>() -> T;
    }
}

mod mem {
    pub unsafe fn uninitialized<T>() -> T {
        intrinsics::uninit()
    }
}

struct Foo(i32, i32);
// { dg-warning "struct is never constructed: .Foo." "" { target *-*-* } .-1 }
// FIXME ^^ above is a bad-warning

impl Foo {
    pub fn new() -> Self {
        unsafe { mem::uninitialized::<Foo>() }
    }
}
