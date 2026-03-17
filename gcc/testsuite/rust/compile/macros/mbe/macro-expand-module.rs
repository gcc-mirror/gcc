#![feature(no_core)]
#![no_core]

mod foo {
    macro_rules! bar {
        () => ()
    }

    bar! ();

    pub struct S;
}

pub fn buzz(_: foo::S) {}
