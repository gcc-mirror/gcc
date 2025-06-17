/* { dg-output "x == y\r*\nx > z\r*\n" }*/
#[lang = "sized"]
pub trait Sized {}

pub enum Option<T> {
    #[lang = "None"]
    None,
    #[lang = "Some"]
    Some(T),
}

use Option::{None, Some};

#[lang = "eq"]
pub trait PartialEq<Rhs: ?Sized = Self> {
    fn eq(&self, other: &Rhs) -> bool;

    fn ne(&self, other: &Rhs) -> bool {
        !self.eq(other)
    }
}

pub enum Ordering {
    Less = -1,
    Equal = 0,
    Greater = 1,
}

#[lang = "partial_ord"]
pub trait PartialOrd<Rhs: ?Sized = Self>: PartialEq<Rhs> {
    fn partial_cmp(&self, other: &Rhs) -> Option<Ordering>;
}

// Implement for i32
impl PartialEq for i32 {
    fn eq(&self, other: &Self) -> bool {
        *self == *other
    }
}

// Implement PartialOrd for i32
impl PartialOrd for i32 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if *self < *other {
            Some(Ordering::Less)
        } else if *self > *other {
            Some(Ordering::Greater)
        } else {
            Some(Ordering::Equal)
        }
    }
}

// Struct with manual PartialEq
struct Foo {
    a: i32,
}

impl PartialEq for Foo {
    fn eq(&self, other: &Self) -> bool {
        self.a.eq(&other.a)
    }
}

impl PartialOrd for Foo {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.a.partial_cmp(&other.a)
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
    let x = Foo { a: 42 };
    let y = Foo { a: 42 };
    let z = Foo { a: 7 };

    match x.partial_cmp(&y) {
        Some(Ordering::Equal) => print("x == y"),
        Some(Ordering::Less) => print("x < y"),
        Some(Ordering::Greater) => print("x > y"),
        None => print("x ? y"),
    }

    match x.partial_cmp(&z) {
        Some(Ordering::Equal) => print("x == z"),
        Some(Ordering::Less) => print("x < z"),
        Some(Ordering::Greater) => print("x > z"),
        None => print("x ? z"),
    }

    0
}
