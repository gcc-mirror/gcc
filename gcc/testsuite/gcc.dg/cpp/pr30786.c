/* PR preprocessor/30786 - _Pragma at end of file should not ICE */
/* { dg-do compile } */

int x;

/* { dg-error "parenthesized" "parenthesized" { target *-*-* } .+2 } */
/* { dg-error "expected" "expected" { target *-*-* } .+1 } */
_Pragma
