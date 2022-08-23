trait Foo {
    fn default() -> i32;
}

trait Bar {
    fn not_default() -> i32;
}

struct Test(i32);

impl Foo for Test {
    fn default() -> i32 {
        1234
    }
}

fn type_bound_test<T: Foo + Bar>() -> i32 {
    T::default()
}

fn main() {
    let a = type_bound_test::<Test>();
    // { dg-error "bounds not satisfied for Test" "" { target *-*-* } .-1 }
}
