pub type T = ();
mod foo {
    pub use super::T;
}

pub use foo::super::foo::S as T;
// { dg-error ".super. in paths can only be used in start position" "" { target *-*-* } .-1 }
