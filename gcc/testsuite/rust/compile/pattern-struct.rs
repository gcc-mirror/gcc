fn main() {
    enum A {
        B,
        C,
    }

    impl A {
        fn new() {}
    }

    fn bar(foo: A) {
        match foo {
            A::new() => (), 
            // { dg-error "expected tuple struct or tuple variant, found function" "" { target *-*-* } .-1 }
            _ => {}
        }
    }
}
