// { dg-additional-options "-frust-name-resolution-2.0 -frust-compile-until=lowering" }

const TOTO: i32 = 10;

fn foo() {
    let b = 10;
    fn bar() {
        let e = TOTO;
    }
}
