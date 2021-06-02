// FIXME: Unimplemented features
fn foo() -> ! { // { dg-error "unresolved type" }
    let a: !; // { dg-error "unresolved type" }
}
