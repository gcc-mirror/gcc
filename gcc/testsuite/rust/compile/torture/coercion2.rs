pub fn main() {
    let a: *const i32 = &123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b: &i32 = &123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let c: &mut i32 = &mut 123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let d: *mut i32 = &mut 123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let e: &i32 = &mut 123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let f: *const i32 = &mut 123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let g = &123;
    let h: *const i32 = g;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
