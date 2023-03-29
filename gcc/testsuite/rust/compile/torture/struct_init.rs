struct Foo {
    one: i32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
    two: i32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn main() {
    let struct_test = Foo { one: 1, two: 2 };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
