static STATIC_1: dyn = *#[serde()]; // { dg-error "found unexpected token .;. in null denotation" "" { target *-*-* } 0 }
