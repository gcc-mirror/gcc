fn foo() -> i32 {
    return 1;
    return 1.5; // { dg-error "expected .i32. got .<float>." }
    // { dg-warning "unreachable statement" "" { target *-*-* } .-1 } 
}

fn bar() -> i32 {
    return 1.5; // { dg-error "expected .i32. got .<float>." }
    return 1;
    // { dg-warning "unreachable statement" "" { target *-*-* } .-1 } 
}

fn main() {
    foo();
    bar();
}
