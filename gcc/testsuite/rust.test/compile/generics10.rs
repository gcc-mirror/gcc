struct Foo<T>(T);

struct Bar<T> {
    a: Foo<T>,
    b: bool,
}

fn test<T>(a: Bar<T>) -> Foo<T> {
    a.a
}

fn main() {
    let a: Bar<i32> = Bar::<i32> {
        a: Foo::<i32>(123),
        b: true,
    };
    let b: Foo<i32> = test(a);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
