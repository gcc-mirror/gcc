#![feature(no_core)]
#![no_core]

// TODO the warnings below shouldn't be emitted...?
struct Foo {
    x: i32, // { dg-bogus "field is never read: .x." TODO { xfail *-*-* } }
    y: i32 // { dg-bogus "field is never read: .y." TODO { xfail *-*-* } }
}

struct Bar (i32, i32);

fn main() {
    let mut my_foo = Foo{x: 32, y: 64};
    let Foo {x, y: _renamed_y} = my_foo; // { dg-warning "unused name .x." }
    let Foo {x: _another_x, ..} = my_foo;
    let Foo {y: _yet_another_y, ..} = my_foo;
    let my_bar = Bar(32, 64);
    let Bar{1: _w, ..} = my_bar;
}
