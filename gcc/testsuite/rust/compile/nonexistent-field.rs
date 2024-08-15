#![allow(unused)]
fn main() {
    struct StructWithFields {
        x: u32,
    }

    let s = StructWithFields { x: 0 };
    s.foo;
    // { dg-error "no field .foo. on type .StructWithFields.StructWithFields .x.u32... .E0609." "" { target *-*-* } .-1 }

    let numbers = (1, 2, 3);
    numbers.3;
    // { dg-error "no field .3. on type ..<integer>, <integer>, <integer>.. .E0609." "" { target *-*-* } .-1 }
}
