struct Foo<A> {
    a: A,
}

impl Foo<isize> {
    fn bar(self) -> isize { // { dg-error "duplicate definitions with name bar" }
        self.a
    }
}

impl Foo<char> {
    fn bar(self) -> char { // { dg-error "duplicate definitions with name bar" }
        self.a
    }
}

impl<T> Foo<T> {
    fn bar(self) -> T {
        self.a
    }
}

fn main() {
    let a = Foo { a: 123 };
    a.bar();
}
