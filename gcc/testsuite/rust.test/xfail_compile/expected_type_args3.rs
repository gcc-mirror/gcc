struct Foo<A>(A);

impl Foo {
    // { dg-error "Invalid number of generic arguments to generic type" "" { target { *-*-* } } .-1 }
    fn test() -> i32 {
        123
    }
}
