#![feature(no_core)]
#![no_core]

trait Foo {
    fn foo();
}

impl Foo for [(); 1] { // { dg-error "missing foo in implementation of trait .Foo. " }
    fn main() {        // { dg-error "method .main. is not a member of trait .Foo." }
        <[(); 0] as Foo>::foo() 
    }
}

fn main() {
    <[(); 0] as Foo>::foo() 
}