#![feature(no_core)]
#![no_core]

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
    // Both variants, as in the reproducer of Rust-GCC/gccrs#3726: a match
    // with only `TypeCtor::Array` is not exhaustive.
    match ty {
        ty_app!(TypeCtor::Array) | ty_app!(TypeCtor::Slice) => {}
    }
}
