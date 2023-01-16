// bogus errors but shows the type checking system needs to support const
// generics with defaults

struct Foo<const N: usize = { 14 }>;

const M: usize = 15;
type N = Foo<3>; // { dg-error "TypePath Foo<> declares generic arguments but the type Foo{Foo {}} does not have any" }

fn main() {
    let _: Foo<15> = Foo; // { dg-error "TypePath Foo<> declares generic arguments but the type Foo{Foo {}} does not have any" }
    let _: Foo<{ M }> = Foo; // { dg-error "TypePath Foo<> declares generic arguments but the type Foo{Foo {}} does not have any" }
    let _: Foo<M> = Foo; // { dg-error "TypePath Foo<> declares generic arguments but the type Foo{Foo {}} does not have any" }

    let _: Foo<N> = Foo; // { dg-error "TypePath Foo<N> declares generic arguments but the type Foo{Foo {}} does not have any" }
}
