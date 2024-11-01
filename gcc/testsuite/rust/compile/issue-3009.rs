#[lang = "sized"]
trait Sized {}

struct Foo {
    // { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
    t: u64,
}

impl Foo {
    fn of<T>() -> Foo {
        // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
        Foo { t: 14 }
    }
}

trait Bar {
    fn bar() -> Foo;
}

impl<T> Bar for T {
    fn bar() -> Foo {
        Foo::of::<T>()
    }
}
