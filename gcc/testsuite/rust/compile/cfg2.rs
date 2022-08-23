// { dg-additional-options "-w -frust-cfg=A" }
struct Foo;
impl Foo {
    #[cfg(not(A))]
    fn test(&self) {}
}

fn main() {
    let a = Foo;
    a.test();
    // { dg-error "failed to resolve method for .test." "" { target *-*-* } .-1 }
    // { dg-error "failed to type resolve expression" "" { target *-*-* } .-2 }
}
