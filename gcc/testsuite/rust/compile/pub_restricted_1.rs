pub mod foo {
    pub mod bar {
        pub fn baz() {}

        pub(in foo::bar) struct A0;
    }
}

pub(in foo::fah::baz) struct A1; // { dg-error "could not resolve path .foo::fah::baz." }
pub(in fro::bulator::saindoux) struct A2; // { dg-error "could not resolve path .fro::bulator::saindoux." }
pub(in foo::bar::saindoux) struct A3; // { dg-error "could not resolve path .foo::bar::saindoux." }

fn main() {}
