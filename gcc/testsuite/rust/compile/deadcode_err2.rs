fn foo() -> i32 {
    return 1;
    return 1.5; // { dg-error "mismatched types, expected .i32. but got .<float>." }
    // { dg-warning "unreachable statement" "" { target *-*-* } .-1 } 
}

fn bar() -> i32 {
    return 1.5; // { dg-error "mismatched types, expected .i32. but got .<float>." }
    return 1;
    // { dg-warning "unreachable statement" "" { target *-*-* } .-1 } 
}

fn main() {
    foo();
    bar();
}
