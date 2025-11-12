macro_rules! ty_app {
    ($_a:pat) => {
        ($ctor)
    };
}

pub fn foo() {
    match ty {
        // { dg-error "Cannot find path" "4140" { target *-*-* } 0 }
        ty_app!(bean::Array) => {} // { dg-error "unrecognised token" "4140" { target *-*-* } 0 }
    }
}
