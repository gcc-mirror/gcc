fn main() {
    let a = 123;
    let b = if a > 10 { a - 1 } else { a + 1 };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
