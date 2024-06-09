// { dg-additional-options "-w -frust-cfg=A" }
struct Foo;
impl Foo {
    #[cfg(not(A))]
    fn test(&self) {}
}

fn main() {
    let a = Foo;
    a.test();
    // { dg-error "no method named .test. found in the current scope" "" { target *-*-* } .-1 }
}
