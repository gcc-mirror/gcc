// https://doc.rust-lang.org/error_codes/E0063.html
struct Foo {
    x: i32,
    y: i32,
    z: i32,
}

fn main() {
    let x = Foo { x: 0 , y:1 }; // { dg-error "constructor is missing fields" }
}