#![feature(decl_macro)]
pub macro hello() [ "Hello" ]
// { dg-error "only braces can be used for a macro transcriber in declarative macro definition" "" { target *-*-* } .-1 }
// { dg-error "failed to parse item in crate" }