// { dg-options "-frust-compile-until=lowering" }

#![feature(decl_macro)]

pub mod foo {
    pub mod bar {
        pub mod baz {
            // macros 2.0 get inserted in Ribs like items
            pub macro boof() {}
        }
    }
}

#[macro_export]
fn main() {
    foo::bar::baz::boof!();
    crate::foo::bar::baz::boof!();
    self::foo::bar::baz::boof!();
}
