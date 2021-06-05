trait Foo {
    fn bar() -> i32;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}

struct Test<T>(T);

impl<T> Foo for Test<T> {
    fn bar() -> i32 {
        123
    }
}

fn main() {
    let a: i32;
    a = Test::<i32>::bar();
}
