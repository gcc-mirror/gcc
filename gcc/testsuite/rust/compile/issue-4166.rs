#![feature(no_core)]
#![no_core]


pub trait Foo {
    type Bar;
    fn foo(bar: Self::bar); // { dg-error "failed to resolve path segment using an impl Probe" }
}

pub struct FooImpl;

const foo_impl: () = {
    impl Foo for FooImpl {
        type Bar = ();
        fn foo(_bar: Self::Bar) { // { dg-error "method .foo. has an incompatible type|mismatched types" }
            // This is the recursive reference that used to cause the ICE
            let () = foo_impl; 
        }
    }
};

fn main() {}