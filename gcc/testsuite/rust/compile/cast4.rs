fn main() {
    let a: i32 = 123;
    let u = a as bool;
    // { dg-error "cannot cast .i32. as .bool." "" { target *-*-* } .-1 }
}
