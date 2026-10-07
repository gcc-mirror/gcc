#![feature(no_core)]
#![no_core]

// A unit variant inside a macro-expanded tuple struct pattern is checked like
// any other, see Rust-GCC/gccrs#3726.

pub enum TypeCtor {
    Slice,
    Array,
}
pub struct ApplicationTy(TypeCtor);

macro_rules! ty_app {
    ($ctor:pat) => {
        ApplicationTy($ctor)
    };
}

pub fn foo(ty: ApplicationTy) {
    match ty {
        // { dg-error "non-exhaustive patterns: 'ApplicationTy . 0: TypeCtor::Slice .' not covered" "" { target *-*-* } .-1 }
        ty_app!(TypeCtor::Array) => {}
    }
}
