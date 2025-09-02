struct Foo<const N: usize = { 14 }>;

const M: usize = 15;
type N = Foo<3>;

fn main() {
    let _: Foo<15> = Foo;
    let _: Foo<{ M }> = Foo;
    let _: Foo<M> = Foo;
    let _: Foo<N> = Foo;
    // { dg-error {type provided when a constant was expected .E0747.} "" { target *-*-* } .-1 }
}
