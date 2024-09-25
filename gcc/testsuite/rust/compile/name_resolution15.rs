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

use self::Foo as Fo;

fn main() {
    let _a = Fo();
    let _b = Fo { a: 15 };
    let _c = Fo!();
}
