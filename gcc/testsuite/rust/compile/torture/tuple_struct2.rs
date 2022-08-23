struct Foo(i32, bool);

fn main() {
    let a = Foo(1, true);

    let b;
    let c;

    b = a.0;
    c = a.1;
}
