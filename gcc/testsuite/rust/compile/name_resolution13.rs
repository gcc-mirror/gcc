// { dg-options "-frust-name-resolution-2.0" }

pub mod foo {
    pub macro bar() {}
}

fn foo() {
    let b = 10;
    fn bar() {
        let c = b;
        // { dg-error "cannot find value .b. in this scope .E0425." "" { target *-*-* } .-1 }
    }
}
