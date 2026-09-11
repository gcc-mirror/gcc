#![feature(no_core)]
#![no_core]
#![feature(lang_items)]

mod clone {
    #[lang = "clone"]
    trait Clone {
        fn clone(&self) -> Self;
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
struct Be<T: clone::Clone> {
    a: T,
    b: Abound,
}

impl clone::Clone for Abound {
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
