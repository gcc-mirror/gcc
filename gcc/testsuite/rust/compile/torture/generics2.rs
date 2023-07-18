#[lang = "sized"]
pub trait Sized {}

struct Foo(f32, bool);

struct GenericStruct<T>(T, usize);

fn main() {
    let a1;
    a1 = Foo(1.0, false);

    let b1: f32 = a1.0;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c1: bool = a1.1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a2: GenericStruct<i8>;
    a2 = GenericStruct::<i8>(1, 456);

    let b2: i8 = a2.0;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c2: usize = a2.1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a3;
    a3 = GenericStruct::<i32>(123, 456);

    let b3: i32 = a3.0;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c3: usize = a3.1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a4;
    a4 = GenericStruct(1.0, 456);

    let b4: f32 = a4.0;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c4: usize = a4.1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a5;
    a5 = GenericStruct::<_>(true, 456);

    let b5: bool = a5.0;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c5: usize = a5.1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
