#![feature(no_core)]
#![no_core]

pub struct TupStruct (i32, i32);

pub fn main() {
    let t = TupStruct (1, 2);
    match t {
        TupStruct { 0: 1, 1: 2, 2: 3 } => {}
        // { dg-error "variant TupStruct does not have a field named 2 .E0026." "" { target *-*-* } .-1 }
        TupStruct { 3: 3 } => {}
        // { dg-error "pattern does not mention fields 0, 1 .E0027." "" { target *-*-* } .-1 }
        // { dg-error "variant TupStruct does not have a field named 3 .E0026." "" { target *-*-* } .-2 }
        TupStruct { a: 3 } => {}
        // { dg-error "tuple variant .TupStruct. written as struct variant .E0769." "" { target *-*-* } .-1 }
        TupStruct { 0: 0.1f32, .. } => {}
        // { dg-error "mismatched types, expected .i32. but got .f32. .E0308." "" { target *-*-* } .-1 }
        _ => {}
    }
}
