pub trait Foo {
    fn Bar(self) -> i32;
    // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
    // { dg-warning "unused name .Bar." "" { target *-*-* } .-2 }
}

struct Baz;
// { dg-warning "struct is never constructed: .Baz." "" { target *-*-* } .-1 }

impl Foo for Baz {
    fn Bar(self) -> i32 {
        // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
        // { dg-warning "unused name .<Baz as Foo>::Bar." "" { target *-*-* } .-2 }
        123
    }
}

fn main() {}
