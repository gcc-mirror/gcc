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
        ty_app!(TypeCtor::Array) => {}
    }
}
