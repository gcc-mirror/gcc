#[repr(packed(2))]
struct Foo {
    x: i16,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
    y: i8,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
    z: i32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

#[repr(packed)]
struct Bar(i8, i32);

fn main () {
    let f = Foo { x: 5, y: 2, z: 13 };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b = Bar (7, 262);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
