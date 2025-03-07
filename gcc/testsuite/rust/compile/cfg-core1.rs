// { dg-additional-options "-frust-cfg=A -frust-cfg=B" }

#[cfg_attr(A, cfg(B))]
struct Foo0;

#[cfg_attr(A, cfg(C))]
struct Bar0;

fn main() {
    let a = Foo0;
    let a = Bar0; // { dg-error "cannot find value" }
}
