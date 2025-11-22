include!(;

struct Baz {

impl Bar for


fn main() { )// { dg-error "unexpected closing delimiter .\\). - token tree requires either paired delimiters or non-delimiter tokens" }
             // { dg-error "failed to parse token tree in delimited token tree - found .\\)." "" { target *-*-* } .-1 }
             // { dg-error "unexpected token .end of file. - expecting closing delimiter .\}. .for a delimited token tree." "" { target *-*-* } .+3 }
             // { dg-error "unexpected token .end of file. - expecting closing delimiter .\\). .for a macro invocation semi." "" { target *-*-* } .+2 }
             // { dg-error "failed to parse item in crate" "" { target *-*-* } .+1 }
