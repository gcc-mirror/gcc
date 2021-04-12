fn test(x: i32) -> i32 {
    return x + 1;
}

fn main() {
    let call_test = test(1);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
