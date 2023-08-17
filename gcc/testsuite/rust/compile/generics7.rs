#[lang = "sized"]
pub trait Sized {}

struct Foo<A> {
    a: A,
}

impl Foo<isize> {
    fn bar(self) -> isize {
        self.a
    }
}

impl Foo<char> {
    fn bar(self) -> char {
        self.a
    }
}

impl<T> Foo<T> {
    fn bar(self) -> T {
        self.a
    }
}
// E0592
fn main() {
    let a = Foo { a: 123 };
    a.bar();
    // { dg-error "duplicate definitions with name .bar." "" { target *-*-* } .-1 }
}
