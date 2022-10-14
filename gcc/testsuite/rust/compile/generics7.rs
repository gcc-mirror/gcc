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

fn main() {
    let a = Foo { a: 123 };
    a.bar();
    // { dg-error "multiple candidates found for method .bar." "" { target *-*-* } .-1 }
    // { dg-error "failed to type resolve expression" "" { target *-*-* } .-2 }
}
