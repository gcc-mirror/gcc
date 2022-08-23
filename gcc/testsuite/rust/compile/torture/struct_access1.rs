struct Foo {
    one: i32,
    two: i32,
}

fn main() {
    let struct_test = Foo { one: 1, two: 2 };
    let a = struct_test.one;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b = struct_test.two;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
