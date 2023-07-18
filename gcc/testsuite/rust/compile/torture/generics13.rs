#[lang = "sized"]
pub trait Sized {}

struct Foo<A> {
    a: A,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

struct GenericStruct<T> {
    a: T,
    b: usize,
}

impl Foo<isize> {
    fn test() -> i32 {
        123
    }

    fn bar(self) -> isize {
        // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
        self.a
    }
}

fn main() {
    let a: i32 = Foo::test();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a2: GenericStruct<i8>;
    a2 = GenericStruct::<i8> { a: 1, b: 456 };

    let b2: i8 = a2.a;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c2: usize = a2.b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let a4;
    a4 = GenericStruct { a: 1.0, b: 456 };

    let b4: f32 = a4.a;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let c4: usize = a4.b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
