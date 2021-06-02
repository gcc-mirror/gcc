struct Foo<T>(T, bool);

impl<T> Foo<T> {
    fn test() -> i32 {
        123
    }
}

fn main() {
    let a = Foo::test();
    // { dg-error "type annotations needed" "" { target *-*-* } .-1 }
}
