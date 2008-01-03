/* PR preprocessor/34602 - no internal error trying to spell EOF.  */
/* { dg-do preprocess } */

/* { dg-error "unexpected end" "" { target *-*-* } 6 } */

#line
