struct Foo(i32, i32);

fn main() {
    let mut a = Foo(1, 2);
    a.0 = 22;
}
