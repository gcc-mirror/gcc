const FOO: u32 = return 0;
// { dg-error "return statement outside of function body" "" { target *-*-* } .-1 }
// { dg-error "mismatched types, expected .u32. but got" "" { target *-*-* } .-2 }
