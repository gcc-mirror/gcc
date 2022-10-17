/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=any,ucn" } */
/* Test \u vs \U.  */

int a_\u202A;
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
int a_\u202a_2;
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
int a_\U0000202A_3;
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
int a_\U0000202a_4;
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
