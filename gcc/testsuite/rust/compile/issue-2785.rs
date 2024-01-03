// { dg-additional-options "-frust-edition=2018" }
trait Foo {
    async fn foo(){}
    // { dg-error "functions in traits cannot be declared .async." "" { target *-*-* } .-1 }
    async fn bar();
    // { dg-error "functions in traits cannot be declared .async." "" { target *-*-* } .-1 }
}

fn main() {}
