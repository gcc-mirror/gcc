#![feature(decl_macro)]
macro m {}
// { dg-error "unexpected token .\}. - expecting delimiters .for a macro matcher." "" { target *-*-* } .-1 }
// { dg-error "required first macro rule in declarative macro definition could not be parsed" "" { target *-*-* } .-2 }
