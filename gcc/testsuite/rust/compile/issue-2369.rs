#[lang = "sized"]
trait Sized {}

fn main() {
    pub trait Foo {
        type A;
        fn boo(&self) -> <Self as Foo>::A;
    }

    struct Bar;

    impl Foo for isize {
        type A = usize;
        fn boo(&self) -> usize {
            42
        }
    }

    fn baz<I>(x: &<I as Foo<A = Bar>>::A) {}
    // { dg-error "associated type bindings are not allowed here .E0229." "" { target *-*-* } .-1 }
}
