// { dg-additional-options "-w -frust-cfg=A=\"B\"" }
struct Foo;
impl Foo {
    #[cfg(A = "B")]
    fn test(&self) {}
}

fn main() {
    let a = Foo;
    a.test();
}
