/* PR c/2161: parser stack overflow.  */
/* { dg-do compile } */
/* { dg-timeout-factor 4.0 } */

#define ONE	else if (0) { }
#define TEN	ONE ONE ONE ONE ONE ONE ONE ONE ONE ONE
#define HUN	TEN TEN TEN TEN TEN TEN TEN TEN TEN TEN
#define THOU	HUN HUN HUN HUN HUN HUN HUN HUN HUN HUN

void foo()
{
  if (0) { }
  /* 11,000 else if's.  */
  THOU THOU THOU THOU THOU THOU THOU THOU THOU THOU THOU
}
