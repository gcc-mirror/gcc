#[lang = "sized"]
pub trait Sized {}

struct S;

impl S {
    fn foo(self) {
        // { dg-warning "infinite recursion detected" "" { target *-*-* } .-1 }
        self.foo();
    }
}

fn main() {
    let a = S;
    a.foo();
}
