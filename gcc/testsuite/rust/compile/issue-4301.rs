#![feature(unused_variables, server = b"\0")]
// { dg-error {unknown feature .server = .} "" { target *-*-* } .-1 }
// { dg-error {unknown feature .unused_variables.} "" { target *-*-* } .-2 }
