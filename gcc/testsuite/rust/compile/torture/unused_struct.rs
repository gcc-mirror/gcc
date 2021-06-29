struct Foo {
    // { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
    // { dg-warning "unused name" "" { target *-*-* } .-2 }
    one: i32,
    two: i32,
}

fn main() {}
