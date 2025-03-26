trait Foo {
    fn f(&self) -> isize;
}

trait Bar: Foo {
    fn g(&self) -> isize;
}

struct A {
    x: isize,
}

impl Bar for A {
    // { dg-error "the trait bound .A: Foo. is not satisfied .E0277." "" { target *-*-* } .-1 }
    fn g(&self) -> isize {
        self.f()
    }
}
