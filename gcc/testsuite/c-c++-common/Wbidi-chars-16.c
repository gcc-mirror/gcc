/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=any,ucn" } */
/* Test LTR/RTL chars.  */

/* LTR<‎> */
/* { dg-warning "U\\+200E" "" { target *-*-* } .-1 } */
// LTR<‎>
/* { dg-warning "U\\+200E" "" { target *-*-* } .-1 } */
/* RTL<‏> */
/* { dg-warning "U\\+200F" "" { target *-*-* } .-1 } */
// RTL<‏>
/* { dg-warning "U\\+200F" "" { target *-*-* } .-1 } */

const char *s1 = "LTR<‎>";
/* { dg-warning "U\\+200E" "" { target *-*-* } .-1 } */
const char *s2 = "LTR\u200e";
/* { dg-warning "U\\+200E" "" { target *-*-* } .-1 } */
const char *s3 = "LTR\u200E";
/* { dg-warning "U\\+200E" "" { target *-*-* } .-1 } */
const char *s4 = "RTL<‏>";
/* { dg-warning "U\\+200F" "" { target *-*-* } .-1 } */
const char *s5 = "RTL\u200f";
/* { dg-warning "U\\+200F" "" { target *-*-* } .-1 } */
const char *s6 = "RTL\u200F";
/* { dg-warning "U\\+200F" "" { target *-*-* } .-1 } */
