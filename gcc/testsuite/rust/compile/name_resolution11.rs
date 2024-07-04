// { dg-additional-options "-frust-name-resolution-2.0 -frust-compile-until=lowering" }
fn foo() {
    let b = 10;
    fn bar() {
        let a = foo;
    }
}
