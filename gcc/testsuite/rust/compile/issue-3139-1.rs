#![feature(lang_items)]

#[lang = "clone"]
trait Clone {
    fn clone(&self) -> Self;
}

#[lang = "sized"]
trait Sized {}

struct Abound {
    a: u32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
    b: u32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

#[derive(Clone)]
struct Be<T:Clone> {
    a: T,
    b: Abound,
}

impl Clone for u32 {
    fn clone(&self) -> Self {
        *self
    }
}

impl Clone for usize {
    fn clone(&self) -> Self {
        *self
    }
}

impl Clone for Abound {
    fn clone(&self) -> Self {
        return Abound { a: self.a.clone(), b: self.b.clone() };
    }
}

fn main() {
    let b: Be<usize> = Be {a:1,b:Abound { a:0,b:1 }};
    let _: Be<usize> = b.clone();
}
