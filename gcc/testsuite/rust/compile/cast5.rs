fn main() {
    const A: char = 0x1F888 as char;
    // { dg-error "cannot cast .<integer>. as .char., only .u8. can be cast as .char." "" { target *-*-* } .-1 }
    const B: char = 129160 as char;
    // { dg-error "cannot cast .<integer>. as .char., only .u8. can be cast as .char." "" { target *-*-* } .-1 }
    const C: i32 = 42;
    const D: char = C as char;
    // { dg-error "cannot cast .i32. as .char., only .u8. can be cast as .char." "" { target *-*-* } .-1 }
    const E: char = '\u{01F888}';
    const F: u8 = 42; 
    const G: char= F as char;
}
