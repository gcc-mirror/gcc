pub fn main() {
    let a: &i32 = &123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b: &mut i32 = &mut 123;

    let c: &i32 = &mut 123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let d: &i32 = b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
