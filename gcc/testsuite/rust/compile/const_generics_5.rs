// { dg-options "-w" }
struct Foo<const N: usize = { 14 }>;

const M: usize = 15;
type N = Foo<3>;

fn main() {
    let _: Foo<15> = Foo;
    let _: Foo<{ M }> = Foo;
    let _: Foo<M> = Foo;
    // let _: Foo<N> = Foo; this causes an ICE we need to do const generics
}
