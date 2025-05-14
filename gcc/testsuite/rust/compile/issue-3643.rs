fn foo() {
    let x: usize<foo>;
    // { dg-error "generic arguments are not allowed for this type .E0109." "" { target *-*-* } .-1 }
}
