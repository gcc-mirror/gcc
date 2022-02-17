macro_rules! Test {
    ($a:ident, $b:ty) => {
        struct $a($b);
    };
}

Test!(Foo, i32);

fn main() -> i32 {
    let a = Foo(123);
    a.0 - 123
}
