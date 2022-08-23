trait Foo {
    fn default() -> i32;
    fn get(self) -> i32;
}

struct Bar(i32);
impl Foo for Bar {
    fn default() -> i32 {
        123
    }

    fn get(self) -> i32 {
        self.0
    }
}

fn type_bound_test<T: Foo>(a: T) -> i32 {
    T::default() + a.get()
}

fn main() {
    let a;
    a = Bar(456);

    let b;
    b = type_bound_test(a);
}
