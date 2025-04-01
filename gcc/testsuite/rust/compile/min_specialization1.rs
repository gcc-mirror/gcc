#![feature(min_specialization)]

pub trait Foo {
    fn foo(&self) -> bool {
        false
    }
}

pub struct Bar;

impl Foo for Bar {
    default fn foo(&self) -> bool { // { dg-warning "unused" }
        true
    }
}
