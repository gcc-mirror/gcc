// { dg-options "-frust-compile-until=lowering" }
#![feature(no_core)]
#![no_core]


pub mod foo {
    pub mod bar {
        pub mod baz {
            pub mod qux {
                #[macro_export]
                macro_rules! foo {
                    (one) => {};
                }

                pub fn foo() {}
            }
        }

        fn f() {
            fn inner() {
                macro_rules! foo {
                    (two) => {};
                }

                foo!(two); // ok, textual scope
                crate::foo!(one); // ok, path res
                super::super::foo!(one); // ok, path res
            }
        }
    }
}
