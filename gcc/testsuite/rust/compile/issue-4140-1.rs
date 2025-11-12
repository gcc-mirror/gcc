pub enum TypeCtor {
    Slice,
    Array,
}
pub struct B<T>(T);

macro_rules! ty_app {
    ($_a:pat) => {
        ApplicationTy($ctor) // { dg-error "unexpected token" }
        // { dg-error "failed to parse tuple struct items" "" { target *-*-* } .-1 }
    };
}

pub fn foo(ty: ApplicationTy) { // { dg-error "could not resolve type path 'ApplicationTy'" }
    match ty {
        ty_app!(bean::Array) => {}
    }
}
