/* -Woverlength-strings complains about string constants which are too long
   for the C standard's "minimum maximum" limits.  It is off by default,
   but implied by -pedantic.  */

/* { dg-options "-std=c99 -pedantic" } */

#define TEN "xxxxxxxxxx"
#define HUN TEN TEN TEN TEN TEN  TEN TEN TEN TEN TEN
#define THO HUN HUN HUN HUN HUN  HUN HUN HUN HUN HUN

/* C89's minimum-maximum is 509. */
const char x510[] = HUN HUN HUN HUN HUN TEN;

/* C99's minimum-maximum is 4095.  */
const char x4096[] =
  THO THO THO THO     /* 4000 */
  TEN TEN TEN TEN TEN /* 4050 */
  TEN TEN TEN TEN     /* 4090 */
  "123456";  /* { dg-warning "greater than" } */
