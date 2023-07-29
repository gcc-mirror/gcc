const FOO: u32 = return 0;
// { dg-error "return statement outside of function body" "" { target *-*-* } .-1 }
// { dg-error "expected .u32. got" "" { target *-*-* } .-2 }
