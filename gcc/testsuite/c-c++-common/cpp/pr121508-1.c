/* PR preprocessor/121508 */
/* { dg-do compile } */

/* { dg-error "'__has_include' used outside of preprocessing directive" "" { target *-*-* } .+4 } */
/* { dg-error "missing terminating '>' character" "" { target *-*-* } .+3 } */
/* { dg-error "missing '\\\)' after '__has_include' operand" "" { target *-*-* } .+2 } */
/* { dg-error "expected ',' or ';' at end of input" "" { target *-*-* } .+1 } */
int i = __has_include(<__int16_t
