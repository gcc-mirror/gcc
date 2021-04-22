struct Foo<A>(A);

fn main() {
    let a: Foo = Foo::<i32>(123);
    // { dg-error "Invalid number of generic arguments to generic type" "" { target { *-*-* } } .-1 }
}
