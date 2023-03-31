fn main() {
    const A: char = 0x1F888 as char;
    // { dg-error "invalid cast .<integer>. to .char." "" { target *-*-* } .-1 }
    const B: char = 129160 as char;
    // { dg-error "invalid cast .<integer>. to .char." "" { target *-*-* } .-1 }
    const C: i32 = 42;
    const D: char = C as char;
    // { dg-error "invalid cast .i32. to .char." "" { target *-*-* } .-1 }
    const E: char = '\u{01F888}';
    const F: u8 = 42; 
    const G: char= F as char;
}
