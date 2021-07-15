struct Foo(i32, bool);

impl Foo {
    fn new(a: i32, b: bool) -> Foo {
    // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
    // { dg-warning "unused name" "" { target *-*-* } .-2 }
        Foo(a, b)
    }

    fn test2() -> i32 {
    // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
    // { dg-warning "unused name" "" { target *-*-* } .-2 }
	1
    }
}

fn main() {
	let _a = Foo(1, true);
}
