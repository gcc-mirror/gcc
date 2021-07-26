fn main() {
    let a: i32 = 123;
    let b = a as char;
    // { dg-error "invalid cast .i32. to .char." "" { target *-*-* } .-1 }
}
