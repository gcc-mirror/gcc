fn main() {}

struct S;

impl S {
    const Y: u8;
    // { dg-error "associated constant in .impl. without body" "" { target *-*-* } .-1 }
}
