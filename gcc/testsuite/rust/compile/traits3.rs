trait Foo {
    type A;

    fn baz(a: Self::A) -> Self::A;
}

struct Bar<T>(T);

impl<T> Foo for Bar<T> {
    type A = i32;

    fn baz(a: f32) -> f32 {
        // { dg-error "method .baz. has an incompatible type for trait .Foo." "" { target *-*-* } .-1 }
        a
        // { dg-error "expected .i32. got .f32." "" { target *-*-* } .-1 }
    }
}

fn main() {
    let a;
    a = Bar::<i32>::baz(123f32);
}
