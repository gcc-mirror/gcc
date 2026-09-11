// { dg-options "-frust-incomplete-and-experimental-compiler-do-not-use" }
#![feature(no_core)]
#![no_core]

#[derive(RustcDecodable)] // { dg-message "is not yet implemented" }
struct Struct1 {}

#[derive(RustcEncodable)] // { dg-message "is not yet implemented" }
struct Struct2 {}

// Pinpoint the global errors (errors with no line number are at line 0)
// { dg-error "could not resolve trait 'RustcDecodable'" "" { target *-*-* } 0 }
// { dg-error "could not resolve trait 'RustcEncodable'" "" { target *-*-* } 0 }