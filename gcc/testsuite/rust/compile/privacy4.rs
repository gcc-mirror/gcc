#[lang = "sized"]
pub trait Sized {}

mod orange {
    mod green {
        fn bean<T>(value: T) -> T {
            value
        }
    }

    fn brown() {// E0603
        green::bean::<bool>(false);
        // { dg-error "definition is private in this context" "" { target *-*-* } .-1 }
        let a = green::bean::<i32>(15);
        // { dg-error "definition is private in this context" "" { target *-*-* } .-1 }

        struct S;

        let s = green::bean(S);
        // { dg-error "definition is private in this context" "" { target *-*-* } .-1 }
    }
}
