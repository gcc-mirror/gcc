fn test(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let a = test(1, 4);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
