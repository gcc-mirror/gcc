const X: &'static str = r#"12
12"#;

BREAK
// { dg-error "unrecognised token" "" { target *-*-* } .-1 }
// { dg-error "failed to parse item" "" { target *-*-* } .-2 }
