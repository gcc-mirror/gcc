struct Foo<const N: usize = { 14 }>;

const M: usize = 15;
type N = Foo<3>;

fn main() {
    let _: Foo<15> = Foo;
    let _: Foo<{ M }> = Foo;
    let _: Foo<M> = Foo;
    // bogus error, but it means the above const generic gets disambiguated properly
    let _: Foo<N> = Foo; // { dg-error "TypePath Foo<N> declares generic arguments but the type Foo{Foo {}} does not have any" }
}
