fn main() {
    let a: i32 = 123;
    let b = a as char;
    // { dg-error "cannot cast .i32. as .char., only .u8. can be cast as .char." "" { target *-*-* } .-1 }
}
