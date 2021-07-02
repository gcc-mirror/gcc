struct Foo {
    one: i32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
    two: i32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn test(x: i32) -> i32 {
    return x + 1;
}

fn main() {
    let logical: bool = true;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let an_integer = 5;
    let mut default_integer = 7;

    default_integer = 1 + an_integer;

    let call_test = test(1);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let struct_test = Foo { one: 1, two: 2 };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
