struct Foo {
    a: f32,
    b: f32,
}

fn main() {
    let a = Foo { 0: 10.0, 1: 20.0 };
    // { dg-error "unknown field .0. .E0560." "" { target *-*-* } .-1 }
    // { dg-error "unknown field .1. .E0560." "" { target *-*-* } .-2 }
}
