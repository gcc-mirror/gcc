// { dg-additional-options "-frust-name-resolution-2.0 -frust-compile-until=lowering" }

fn foo() {
    let b = 10;
    fn bar() {
        let c = b;
        // { dg-error "cannot find value .b. in this scope .E0425." "" { target *-*-* } .-1 }
    }
}
