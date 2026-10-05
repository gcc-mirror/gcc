// Operands of a call, a method call and an overloaded operator are
// evaluated from left to right.
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[lang = "add"]
trait Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}

static mut N: i32 = 0;

fn tick() -> i32 {
    unsafe {
        N += 1;
        N
    }
}

fn three(a: i32, b: i32, c: i32) -> i32 {
    a * 100 + b * 10 + c
}

struct S;

impl S {
    fn two(&self, a: i32, b: i32) -> i32 {
        a * 10 + b
    }
}

struct W(i32);

impl Add for W {
    type Output = i32;
    fn add(self, rhs: W) -> i32 {
        self.0 * 10 + rhs.0
    }
}

fn w() -> W {
    W(tick())
}

fn main() -> i32 {
    if three(tick(), tick(), tick()) != 123 {
        return 1;
    }

    unsafe { N = 0 };
    let s = S;
    if s.two(tick(), tick()) != 12 {
        return 2;
    }

    unsafe { N = 0 };
    if w() + w() != 12 {
        return 3;
    }

    0
}
