fn main() {
    let a = &123;
    let b: &mut i32 = a;
    // { dg-error "mismatched mutability" "" { target *-*-* } .-1 }
    // { dg-error "expected .&mut i32. got .& i32." "" { target *-*-* } .-2 }
}
