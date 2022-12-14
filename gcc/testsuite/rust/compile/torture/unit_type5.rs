struct Foo;

fn main() {
    let a = Foo {};
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b = Foo;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
