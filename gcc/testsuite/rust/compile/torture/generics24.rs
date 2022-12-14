struct Foo<A = (isize, char)> {
    a: A,
}

impl Foo<isize> {
    fn bar(self) -> isize {
        self.a
    }
}

impl Foo<char> {
    fn bar(self) -> char {
        // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
        self.a
    }
}

impl Foo {
    fn bar(self) {
        let a: (isize, char) = self.a;
        let b = a.0;
        let c = a.1;
        // { dg-warning "unused name" "" { target *-*-* } .-1 }

        let aa: Foo<isize> = Foo { a: b };
        let bb: isize = aa.bar();
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
    }
}

fn main() {
    let a = Foo { a: (123, 'a') };
    a.bar();
}
