struct Foo(f32, i32);

fn main() {
    let a = Foo(1, 2);
    a.0 = 22;
}
