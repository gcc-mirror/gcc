pub mod foo {
    pub mod bar {
        pub fn baz() {}
    }
}

// this is invalid Rust: We just want to make sure the paths get resolved properly
pub(in foo::bar::baz) struct A0;

pub(in foo::fah::baz) struct A1; // { dg-error "cannot find simple path segment .fah." }
pub(in fro::bulator::saindoux) struct A2; // { dg-error "cannot find simple path segment .fro." }
pub(in foo::bar::saindoux) struct A3; // { dg-error "cannot find simple path segment .saindoux." }

fn main() {}
