struct S<const N: usize>;

pub fn foo<const N: FooBar>() {} // { dg-error "could not resolve" }
type Foo<const N: FooBar> = S<N>; // { dg-error "could not resolve" }
struct Foo2<const N: FooBar>; // { dg-error "could not resolve" }
enum Foo3<const N: FooBar> { // { dg-error "could not resolve" }
    Foo,
    Bar,
}
union Foo4<const N: FooBar> { // { dg-error "could not resolve" }
    a: usize,
    b: i32,
}
trait Fooable<const N: FooBar> {} // { dg-error "could not resolve" }

trait Traitable {}
impl<const N: FooBar> Traitable for Foo2<N> {} // { dg-error "could not resolve" }
