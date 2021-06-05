trait Foo {
    fn bar() -> i32;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}

struct Test(i32, f32);

impl Foo for Test {
    fn bar() -> i32 {
        123
    }
}

fn main() {
    let a: i32;
    a = Test::bar();
}
