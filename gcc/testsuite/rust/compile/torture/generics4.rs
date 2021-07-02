struct Foo<T> {
    a: T,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
    b: bool,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn test<T>(a: T) -> Foo<T> {
    Foo { a: a, b: true }
}

fn main() {
    let a: Foo<i32> = test(123);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b: Foo<u32> = test(456);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
