struct Foo<T>(T, bool);

impl Foo<i32> {
    fn new() -> Self {
        Foo(123, true)
    }

    fn bar(self) -> i32 {
        self.0
    }
}

impl Foo<f32> {
    fn new() -> Self {
        Foo(123f32, true)
    }

    fn bar(self) -> f32 {
        self.0
    }
}

fn main() {
    let a = Foo::<i32>::new();
    let aa: i32 = a.bar();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let b = Foo::<f32>::new();
    let bb: f32 = b.bar();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
