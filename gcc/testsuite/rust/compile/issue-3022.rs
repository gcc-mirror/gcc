#[lang = "sized"]
trait Sized {}

trait Foo<T> {
    fn foo(self) -> T;
}

struct Bar<T, U> {
    // { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
    value: U,
    valte: T,
}

impl<T: Foo<U>, U> Foo<U> for Bar<T, U> {
    fn foo(self) -> U {
        self.value
    }
}
