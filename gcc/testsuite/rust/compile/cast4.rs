fn main() {
    let a: i32 = 123;
    let u = a as bool;
    // { dg-error "invalid cast .i32. to .bool." "" { target *-*-* } .-1 }
}
