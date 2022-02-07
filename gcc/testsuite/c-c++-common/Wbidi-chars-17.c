/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=unpaired,ucn" } */
/* Test LTR/RTL chars.  */

/* LTR<‎> */
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
// LTR<‎>
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
/* RTL<‏> */
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
// RTL<‏>
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
int ltr_\u200e;
/* { dg-error "universal character " "" { target *-*-* } .-1 } */
int rtl_\u200f;
/* { dg-error "universal character " "" { target *-*-* } .-1 } */

const char *s1 = "LTR<‎>";
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
const char *s2 = "LTR\u200e";
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
const char *s3 = "LTR\u200E";
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
const char *s4 = "RTL<‏>";
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
const char *s5 = "RTL\u200f";
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
const char *s6 = "RTL\u200F";
/* { dg-bogus "unpaired" "" { target *-*-* } .-1 } */
