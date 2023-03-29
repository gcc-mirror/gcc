// { dg-additional-options "-w" }

mod foo {
    mod bar {
        pub(in foo) fn baz() {}
    }

    fn baz() {
        bar::baz(); // no error, foo::bar::baz is public in foo
    }
}
