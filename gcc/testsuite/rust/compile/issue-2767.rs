// { dg-additional-options "-frust-edition=2018" }
trait Foo {
    fn f() -> u32;
}

impl Foo for u32 {
    async fn f() -> u32 {
        // { dg-error "functions in traits cannot be declared .async." "" { target *-*-* } .-1 }
        22
    }
}

fn main() {}
