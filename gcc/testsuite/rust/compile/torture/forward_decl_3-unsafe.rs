fn main() {
    unsafe {
        let struct_test = Foo { one: 1, two: 2 };
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
    };
}

struct Foo {
    one: i32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
    two: i32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}
