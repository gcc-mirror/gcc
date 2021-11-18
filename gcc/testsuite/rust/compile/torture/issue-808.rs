pub trait Foo {
    type Target;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    fn bar(&self) -> &Self::Target;
    // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
    // { dg-warning "unused name .Foo::bar." "" { target *-*-* } .-2 }
}

impl<T> Foo for &T {
    type Target = T;

    fn bar(&self) -> &T {
        *self
    }
}

pub fn main() {
    let a: i32 = 123;
    let b: &i32 = &a;

    b.bar();
}
