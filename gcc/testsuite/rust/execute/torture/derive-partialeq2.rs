// { dg-output "true\r*\nfalse\r*\nfalse\r*\nfalse\r*\nfalse\r*\n" }

#![feature(intrinsics)]

#[lang = "sized"]
trait Sized {}

#[lang = "copy"]
trait Copy {}

#[lang = "structural_peq"]
trait StructuralPartialEq {}

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

#[derive(PartialEq)]
enum Foo {
    A { a: i32, b: i32 },
    B(i32, i32),
    C,
}

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
    let x = Foo::A { a: 15, b: 14 };

    let b1 = x == Foo::A { a: 15, b: 14 };
    let b12 = x == Foo::A { a: 15, b: 19 };
    let b13 = x == Foo::A { a: 19, b: 14 };
    let b2 = x == Foo::B(15, 14);
    let b3 = x == Foo::C;

    print(b1);
    print(b12);
    print(b13);
    print(b2);
    print(b3);

    0
}
