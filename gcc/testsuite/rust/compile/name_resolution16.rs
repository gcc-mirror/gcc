// { dg-additional-options "-frust-name-resolution-2.0" }
#![feature(decl_macro)]

pub mod foo {
    pub struct Foo {
        pub a: i32,
    }
    pub fn Foo() {}
    pub macro Foo() {{}}
}

pub use foo::Foo;

fn main() {
    let _a = Foo();
    let _b = Foo { a: 15 };
    let _c = Foo!();
}
