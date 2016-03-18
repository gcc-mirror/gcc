/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ira" } */

/* { dg-final { scan-rtl-dump "Adding REG_EQUIV to insn \[0-9\]+ for source of insn \[0-9\]+" "ira" } } */

typedef float XFtype __attribute__ ((mode (XF)));
typedef _Complex float XCtype __attribute__ ((mode (XC)));
XCtype
__mulxc3 (XFtype a, XFtype b, XFtype c, XFtype d)
{
  XFtype ac, bd, ad, bc, x, y;
  ac = a * c;
__asm__ ("": "=m" (ac):"m" (ac));
  if (x != x)
    {
      _Bool recalc = 0;
      if (((!(!(((ac) - (ac)) != ((ac) - (ac)))))))
	recalc = 1;
      if (recalc)
	x = __builtin_huge_vall () * (a * c - b * d);
    }
  return x;
}
