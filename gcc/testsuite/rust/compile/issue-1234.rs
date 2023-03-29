fn foo() -> u8 {
    // { dg-warning "function is never used" "" { target *-*-* } .-1 }
    1u8 << 2u32
}
