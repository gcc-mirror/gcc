#[lang = "sized"]
trait Sized {}

trait Foo {
    fn id(&self) -> i32;
}

struct A;
struct B;

impl Foo for A {
    fn id(&self) -> i32 {
        1
    }
}

impl Foo for B {
    fn id(&self) -> i32 {
        2
    }
}

fn make_foo(cond: bool) -> impl Foo {
    if cond { A } else { B }
    // { dg-error "mismatched types, expected .A. but got .B. .E0308." "" { target *-*-* } .-1 }
}
