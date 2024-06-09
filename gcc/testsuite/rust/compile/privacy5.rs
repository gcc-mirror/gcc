mod orange {
    mod green {
        struct Foo;
        pub(in orange) struct Bar;
        pub struct Baz;
    }

    fn brown() {// E0603
        let _ = green::Foo; // { dg-error "definition is private in this context" }
        let _ = green::Bar;
        let _ = green::Baz;

        let _: green::Foo; // { dg-error "definition is private in this context" }

        fn any(a0: green::Foo, a1: green::Bar) {} // { dg-error "20:definition is private in this context" }
    }
}
