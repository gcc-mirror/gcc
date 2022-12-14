// { dg-additional-options "-w -frust-cfg=A" }
struct Foo;
impl Foo {
    #[cfg(any(A, B))]
    fn test(&self) {}
}

fn main() {
    let a = Foo;
    a.test();
}
