// { dg-additional-options "-frust-compile-until=lowering" }
fn foo() {
    let b = 10;
    fn bar() {
        let a = foo;
    }
}
