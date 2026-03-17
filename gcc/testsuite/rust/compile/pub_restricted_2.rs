// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]


mod foo {
    mod bar {
        mod baz {
            pub(in super::baz) struct A0;
            pub(in super::super::bar::baz) struct A1;
            pub(in foo::bar::baz) struct A2;

            mod sain {
                mod doux {}
            }

            pub(in self::sain) struct A3; // { dg-error "restricted path is not an ancestor of the current module" }
            pub(in self::sain::doux) struct A4; // { dg-error "restricted path is not an ancestor of the current module" }
        }
    }
}
