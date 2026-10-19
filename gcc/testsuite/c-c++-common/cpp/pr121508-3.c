/* PR preprocessor/121508 */
/* { dg-do compile } */

/* { dg-error "'__has_embed' used outside of preprocessing directive" "" { target *-*-* } .+4 } */
/* { dg-error "missing terminating '>' character" "" { target *-*-* } .+3 } */
/* { dg-error "expected '\\\)'" "" { target *-*-* } .+2 } */
/* { dg-error "expected ',' or ';' at end of input" "" { target *-*-* } .+1 } */
int i = __has_embed(<__int16_t
