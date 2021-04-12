fn test(x: f32) -> f32 {
    return x + 1.0;
}

fn main() {
    let a_float = 5.123;
    let call_test = test(a_float + 1.0);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
