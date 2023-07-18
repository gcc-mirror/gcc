#[lang = "sized"]
pub trait Sized {}

struct Foo {
    a: f32,
    b: bool,
}

struct GenericStruct<T> {
    a: T,
    b: usize,
}

fn main() {
    let a1;
    a1 = Foo { a: 1.0, b: false };

    let b1: f32 = a1.a;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c1: bool = a1.b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a2: GenericStruct<i8>;
    a2 = GenericStruct::<i8> { a: 1, b: 456 };

    let b2: i8 = a2.a;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c2: usize = a2.b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a3;
    a3 = GenericStruct::<i32> { a: 123, b: 456 };

    let b3: i32 = a3.a;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c3: usize = a3.b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a4;
    a4 = GenericStruct { a: 1.0, b: 456 };

    let b4: f32 = a4.a;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c4: usize = a4.b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a5;
    a5 = GenericStruct::<_> { a: true, b: 456 };

    let b5: bool = a5.a;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c5: usize = a5.b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
