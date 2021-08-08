trait Foo {
    fn default() -> i32;
}

struct Bar(i32);

fn type_bound_test<T: Foo>() -> i32 {
    T::default()
}

fn main() {
    let a;
    a = type_bound_test::<Bar>();
    // { dg-error "bounds not satisfied for Bar" "" { target *-*-* } .-1 }
}
