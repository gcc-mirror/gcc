// github issue #415
fn test<A, B>(a: A, b: B) -> (A, B) {
    (a, b)
}

fn main() {
    let a = test::<i32, i32>(123, 456);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let b = test::<f32, f32>(123f32, 456f32);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let c = test::<_, _>(123, 456f32);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let d = test(true, 1234);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let e = test((123, false), 123f32);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
