// https://doc.rust-lang.org/error_codes/E0063.html
struct Foo {
    x: i32,
    y: i32,
    z: i32,
}

fn main() {
    let z = Foo { x: 0 , y:1 }; // { dg-error "missing field z in initializer of 'Foo'" }
    let xz = Foo { y:1 }; // { dg-error "missing fields x, z in initializer of 'Foo'" }
    let xyz = Foo { }; // { dg-error "missing fields x, y, z in initializer of 'Foo'" }
}
