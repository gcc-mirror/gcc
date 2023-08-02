fn foo() -> i32 {
    return 1;

    let mut a = 1; // { dg-warning "unreachable statement" }
    a = 1.1; // { dg-warning "unreachable statement" }
    // { dg-error "mismatched types, expected .<integer>. but got .<float>." "" { target *-*-* } .-1 }
}

fn main() {
    foo();
}
