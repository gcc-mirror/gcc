/* PR preprocessor/30786 - _Pragma at end of file should not ICE */
/* { dg-do compile } */

/* { dg-error "parenthesized" "" { target *-*-* } 9 } */
/* { dg-error "expected" "" { target *-*-* } 9 } */

int x;

_Pragma
