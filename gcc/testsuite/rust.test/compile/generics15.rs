struct Foo<T>(T, bool);

impl Foo<i32> {
    fn bar(self) -> i32 {
        self.0
    }
}

impl Foo<f32> {
    fn bar(self) -> f32 {
        self.0
    }
}

fn main() {
    let a = Foo(123, true);
    let aa = a.bar();

    let b = Foo(456f32, true);
    let bb = b.bar();
}
