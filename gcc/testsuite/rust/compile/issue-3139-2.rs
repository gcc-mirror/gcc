#![feature(lang_items)]

#[lang = "clone"]
trait Clone {
    fn clone(&self) -> Self;
}

#[lang = "sized"]
trait Sized {}

struct Abound {
    a: u32,
    b: u32,
}

struct Be<T: Clone> {
    a: T,
    b: Abound,
}

impl<T: Clone> Clone for Be<T> {
    fn clone(&self) -> Self {
        return Be::<T> {
            a: self.a.clone(),
            b: self.b.clone(),
        };
    }
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
        return Abound {
            a: self.a.clone(),
            b: self.b.clone(),
        };
    }
}

fn main() {
    let b: Be<usize> = Be {
        a: 1,
        b: Abound { a: 0, b: 1 },
    };
    let _: Be<usize> = b.clone();
}
