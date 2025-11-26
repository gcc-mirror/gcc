const X: &'static u8 = br#"12
12"#;

BREAK
// { dg-error "unrecognised token" "" { target *-*-* } .-1 }
