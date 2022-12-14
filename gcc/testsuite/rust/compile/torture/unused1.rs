fn test() -> i32 {
    1
}

fn unused() -> i32 {
    // { dg-warning "function is never used: 'unused'" "" { target *-*-* } .-1 }
    2
}

fn main() {
    let a = 1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b = test();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
