#[lang = "sized"]
pub trait Sized {}

pub struct A(i32, i32);

trait Clone {
    fn clone(&self) -> Self;
}

impl Clone for A {
    fn clone(&self) -> A {
        A(self.0, self.1)
    }
}

pub fn foo(c: bool, d: i32) {
    { (0, 1) }.0 + 1;
    if c { A(4, 5) } else { A(12, 23) }.clone().0 * 5;
    match d { 1 => A(-5, 0), _ => A(-23, -6) }.clone();
    match d {
        _ => { (4, 5) }.1 / 3,
    };
}
