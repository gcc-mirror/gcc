const X: &'static str = r#"12
12"#;

BREAK
// { dg-error "unrecognised token" "" { target *-*-* } .-1 }
// { dg-excess-errors "error 'failed to parse item' does not have location" }
