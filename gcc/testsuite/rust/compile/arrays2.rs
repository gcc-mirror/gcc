// { dg-additional-options "-w" }
fn main() {
    let array: [i32; 5] = [1, 2, 3];
    // { dg-error "mismatched types, expected an array with a fixed size of 5 elements, found one with 3 elements" "" { target *-*-* } .-1 }
}
