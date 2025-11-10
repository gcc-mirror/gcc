pub enum TypeCtor {
    Slice,
    Array,
}
pub struct B<T>(T);

macro_rules! ty_app {
    ($_a:pat) => {
        ApplicationTy($ctor) // { dg-error "unexpected token '$' in typle struct items" "4140" { target *-*-* } . }
        // { dg-error "failed to parse typle struct items" "4140" { target *-*-*} .-1 }
    };
}

pub fn foo(ty: ApplicationTy) { // { dg-error "could not resolve type path 'ApplicationTy'" "4140" { target *-*-* } .-1 }
    match ty {
        ty_app!(bean::Array) => {}
    }
}
