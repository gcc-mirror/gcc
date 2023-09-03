mod orange {
    mod green {
        fn sain_void() {}
        fn sain() -> bool {
            false
        }
        pub fn doux() {}
    }

    fn brown() {
        if green::sain() {// E0603
            // { dg-error "definition is private in this context" "" { target *-*-* } .-1 }
            green::doux();
        }

        {
            green::sain();
            // { dg-error "definition is private in this context" "" { target *-*-* } .-1 }
            green::sain();
            // { dg-error "definition is private in this context" "" { target *-*-* } .-1 }
            green::sain_void()
            // { dg-error "definition is private in this context" "" { target *-*-* } .-1 }
        }

        let a = green::sain();
        // { dg-error "definition is private in this context" "" { target *-*-* } .-1 }
    }
}
