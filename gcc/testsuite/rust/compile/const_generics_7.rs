struct S<const N: usize>;

pub fn foo<const N: FooBar>() {} // { dg-error "failed to resolve" }
type Foo<const N: FooBar> = S<N>; // { dg-error "failed to resolve" }
struct Foo2<const N: FooBar>; // { dg-error "failed to resolve" }
enum Foo3<const N: FooBar> { // { dg-error "failed to resolve" }
    Foo,
    Bar,
}
union Foo4<const N: FooBar> { // { dg-error "failed to resolve" }
    a: usize,
    b: i32,
}
trait Fooable<const N: FooBar> {} // { dg-error "failed to resolve" }

trait Traitable {}
impl<const N: FooBar> Traitable for Foo2<N> {} // { dg-error "failed to resolve" }
