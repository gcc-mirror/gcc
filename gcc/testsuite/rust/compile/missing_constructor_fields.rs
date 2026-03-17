// https://doc.rust-lang.org/error_codes/E0063.html
#![feature(no_core)]
#![no_core]

struct Foo {
    x: i32,
    y: i32,
    z: i32,
}

fn main() {
    let z = Foo { x: 0 , y:1 }; // { dg-error "missing field 'z' in initializer of 'Foo'" }
    let xz = Foo { y:1 }; // { dg-error "missing fields 'x' and 'z' in initializer of 'Foo'" }
    let xyz = Foo { }; // { dg-error "missing fields 'x', 'y' and 'z' in initializer of 'Foo'" }
}
