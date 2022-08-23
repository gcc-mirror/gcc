fn test(x: i32) -> i32 {
    return x + 1;
}

fn main() {
    let mut an_integer = 5;
    an_integer = test(1) + 3;

    let mut x;
    x = 1;

    let call_test = test(1);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
