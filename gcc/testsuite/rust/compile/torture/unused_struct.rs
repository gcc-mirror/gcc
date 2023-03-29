struct Foo {
    // { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
    one: i32,
    two: i32,
}

fn main() {}
