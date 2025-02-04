// { dg-output "true\r*\nfalse\r*\nfalse\r*\n" }

#![feature(intrinsics)]

#[lang = "sized"]
trait Sized {}

#[lang = "copy"]
trait Copy {}

#[lang = "eq"]
pub trait PartialEq<Rhs: ?Sized = Self> {
    /// This method tests for `self` and `other` values to be equal, and is used
    /// by `==`.
    #[must_use]
    #[stable(feature = "rust1", since = "1.0.0")]
    fn eq(&self, other: &Rhs) -> bool;

    /// This method tests for `!=`.
    #[inline]
    #[must_use]
    #[stable(feature = "rust1", since = "1.0.0")]
    fn ne(&self, other: &Rhs) -> bool {
        !self.eq(other)
    }
}

#[lang = "structural_peq"]
trait StructuralPartialEq {}

#[derive(PartialEq, Copy)] // { dg-warning "unused name" }
struct Foo;

#[derive(PartialEq)]
struct Bar(Foo);

#[derive(PartialEq)]
struct Baz { _inner: Foo }

extern "C" {
    fn puts(s: *const i8);
}

fn print(b: bool) {
    if b {
        unsafe { puts("true" as *const str as *const i8) }
    } else {
        unsafe { puts("false" as *const str as *const i8) }
    }
}

fn main() -> i32 {
    let x = Foo;

    let b1 = x == Foo;
    let b2 = Bar(x) != Bar(Foo);
    let b3 = Baz { _inner: Foo } != Baz { _inner: x };

    print(b1);
    print(b2);
    print(b3);

    0
}
