/* { dg-output "a == b\r*\na != c\r*\n" }*/
/* { dg-options "-w" } */

#[lang = "sized"]
pub trait Sized {}

#[lang = "eq"]
pub trait PartialEq<Rhs: ?Sized = Self> {
    fn eq(&self, other: &Rhs) -> bool;

    fn ne(&self, other: &Rhs) -> bool {
        !self.eq(other)
    }
}

impl PartialEq for i32 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

struct Foo<T> {
    value: T,
}

impl<T: PartialEq> PartialEq for Foo<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

extern "C" {
    fn puts(s: *const i8);
}

fn print(s: &str) {
    unsafe {
        puts(s as *const str as *const i8);
    }
}

fn main() -> i32 {
    let a = Foo { value: 42i32 };
    let b = Foo { value: 42i32 };
    let c = Foo { value: 99i32 };

    if a == b {
        print("a == b");
    } else {
        print("a != b");
    }

    if a == c {
        print("a == c");
    } else {
        print("a != c");
    }

    0
}
