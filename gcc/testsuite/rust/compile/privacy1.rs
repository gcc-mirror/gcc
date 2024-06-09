mod orange {
    mod green {
        fn sain() {}
        pub fn doux() {}
    }

    fn brown() {// E0603
        green::sain(); // { dg-error "definition is private in this context" }
        green::doux();
    }
}
