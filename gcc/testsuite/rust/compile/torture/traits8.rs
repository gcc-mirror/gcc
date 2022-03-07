trait Foo {
    fn default() -> i32;
}

struct Bar(i32);
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }

impl Foo for Bar {
    fn default() -> i32 {
        123
    }
}

fn type_bound_test<T: Foo>() -> i32 {
    T::default()
}

fn main() {
    let a;
    a = type_bound_test::<Bar>();
}
