#[lang = "sized"]
pub trait Sized {}

struct Foo<A> {
    a: A,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

impl Foo<isize> {
    fn test() -> i32 {
        123
    }

    fn bar(self) -> isize {
        // { dg-warning "associated function is never used" "" { target *-*-* } .-1 }
        self.a
    }
}

fn main() {
    let a: i32 = Foo::test();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
