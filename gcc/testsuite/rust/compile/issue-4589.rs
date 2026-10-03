// { dg-additional-options "-frust-edition=2015" }

#![feature(no_core)]
#![no_core]

pub mod foo {
    pub fn bar() {}
}

pub mod baz {
    pub use foo::bar;

    pub mod inner {
        pub use self::nested::f;
        pub use super::bar as g;

        pub mod nested {
            pub fn f() {}
        }
    }
}

pub mod qux {
    pub use foo::*;
}

pub mod quux {
    use *;

    pub fn h() {
        foo::bar();
    }
}

fn main() {
    baz::bar();
    baz::inner::f();
    baz::inner::g();
    qux::bar();
    quux::h();
}
