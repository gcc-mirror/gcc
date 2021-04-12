fn test(x: i32) -> i32 {
    return x + 1;
}

fn main() {
    let an_integer = 5;
    let call_test = test(an_integer + 1);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
