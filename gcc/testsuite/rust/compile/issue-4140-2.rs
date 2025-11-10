macro_rules! ty_app {
    ($_a:pat) => {
        ($ctor) // { dg-error "unrecognised token '$' in grouped or tuple pattern after first pattern" "4140" { target *-*-* } . }
    };
}

pub fn foo() {
    match ty { // { dg-error "Cannot find path 'ty' in this scope" "4140" { target *-*-* } .-1 }
        ty_app!(bean::Array) => {}
    }
}
