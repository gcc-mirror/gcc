/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "-std=c++11" { target c++ } } */

/* { dg-error "invalid new-line in raw string delimiter" "" { target *-*-* } .+4 } */
/* { dg-error "unterminated raw string" "" { target *-*-* } .+3 } */
/* { dg-error "stray 'R' in program" "" { target *-*-* } .+2 } */
/* { dg-warning "expected a string" "" { target *-*-* } .+1 } */
#pragma message R""
