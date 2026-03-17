#![feature(no_core)]
#![no_core]

const X: &'static str = r#"12
12"#;

BREAK
// { dg-error "unrecognised token" "" { target *-*-* } .-1 }
