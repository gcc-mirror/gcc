// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]


mod foo {
    mod bar {
        pub(in foo) fn baz() {}
    }

    fn baz() {
        bar::baz(); // no error, foo::bar::baz is public in foo
    }
}
