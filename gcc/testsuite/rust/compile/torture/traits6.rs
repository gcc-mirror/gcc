trait Foo {
    type A;
    // { dg-warning "unused name .Foo::A." "" { target *-*-* } .-1 }

    fn baz(a: Self::A) -> Self::A;
    // { dg-warning "unused name .a." "" { target *-*-* } .-1 }
    // { dg-warning "unused name .Foo::baz." "" { target *-*-* } .-2 }
}

struct Bar<T>(T);

impl<T> Foo for Bar<T> {
    type A = T;

    fn baz(a: Self::A) -> T {
        a
    }
}

fn main() {
    let a;
    a = Bar::<i32>::baz(123);
}
