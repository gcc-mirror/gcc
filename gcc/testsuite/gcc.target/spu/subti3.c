/* { dg-do run } */
/* { dg-options "-std=c99" } */
#include <stdlib.h>
typedef int TItype __attribute__ ((mode (TI)));
typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));

struct DIstruct {DItype high, low;};
typedef union
{
  struct DIstruct s;
  TItype t;
} TIunion;

static
void sub_ddmmss (UDItype *sh, UDItype *sl, UDItype ah, UDItype al, UDItype bh, UDItype bl)
{
  UDItype x;
  x = al - bl;
  *sh = ah - bh - (x > al);
  *sl = x;
}

int main(void)
{
  TIunion aa, bb, cc;
  TItype m = 0x1111111111111110ULL;
  TItype n = 0x1111111111111111ULL;
  TItype d;

  aa.s.high = m;
  aa.s.low = m;
  bb.s.high = n;
  bb.s.low = n;


  sub_ddmmss (&cc.s.high, &cc.s.low, aa.s.high, aa.s.low, bb.s.high, bb.s.low);
  d = aa.t - bb.t;
  if (d != cc.t)
   abort();
  cc.t = aa.t -d;
  if (cc.t != bb.t)
   abort();
 return 0;
}
