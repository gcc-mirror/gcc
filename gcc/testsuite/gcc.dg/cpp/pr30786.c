/* PR preprocessor/30786 - _Pragma at end of file should not ICE */
/* { dg-do compile } */

/* { dg-error "parenthesized" "parenthesized" { target *-*-* } 9 } */
/* { dg-error "expected" "expected" { target *-*-* } 9 } */

int x;

_Pragma
