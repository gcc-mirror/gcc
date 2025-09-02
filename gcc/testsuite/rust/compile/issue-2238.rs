#[lang = "sized"]
pub trait Sized {}

fn main() {
    struct Foo;

    trait Bar {
        fn foo(&self);
    }

    impl Bar for Foo {
        fn foo(&self) {}
    }

    let s = Foo;
    s.foo();
}
