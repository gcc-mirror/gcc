#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

struct Two<'a> { // { dg-warning "struct is never constructed: .Two." }
    f: &'a i32,
    g: &'a i32,
}

fn a() { // { dg-warning "function is never used: .a." }
    let (mut t, f, g): (Two, i32, i32);
    f = 42;
    t = Two { f: &f, g: &f };
    *t.f;
    f = 42; // { dg-error "assignment of read-only variable 'f'" }
    t.g = &g;
}
