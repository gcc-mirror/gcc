/* PR preprocessor/123339 */
/* { dg-do compile } */

/* { dg-error "'__has_include' used outside of preprocessing directive" "" { target *-*-* } .+5 } */
/* { dg-error "missing terminating '>' character" "" { target *-*-* } .+4 } */
/* { dg-error "missing '\\\(' before '__has_include' operand" "" { target *-*-* } .+3 } */
/* { dg-error "expected identifier or '\\\(' before numeric constant" "" { target c } .+2 } */
/* { dg-error "expected unqualified-id before numeric constant" "" { target c++ } .+1 } */
__has_include <"foo"
