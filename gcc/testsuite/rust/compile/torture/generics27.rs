// github issue #415
fn test<A>(a: &A) -> &A {
    a
}

fn main() {
    let a = 123;
    let b = &a;
    let c = test(b);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a = 123f32;
    let b = &a;
    let c = test(b);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
