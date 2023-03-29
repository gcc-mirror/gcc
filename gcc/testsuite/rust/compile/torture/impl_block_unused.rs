struct Foo(i32, bool);

impl Foo {
    fn new(a: i32, b: bool) -> Foo {
        // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
        Foo(a, b)
    }

    fn test2() -> i32 {
        // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
        1
    }
}

fn main() {
    let _a = Foo(1, true);
}
