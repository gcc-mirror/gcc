#![feature(lang_items)]

#[lang = "copy"]
trait Copy {}

#[lang = "sized"]
trait Sized {}

#[derive(Copy)]
struct Abound {
    a: u32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
    b: u32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

#[derive(Copy)]
struct Be<T: Copy> {
    a: T,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
    b: Abound,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

impl Copy for usize {}

fn main() {
    let _: Be<usize> = Be {
        a: 1,
        b: Abound { a: 0, b: 1 },
    };
}
