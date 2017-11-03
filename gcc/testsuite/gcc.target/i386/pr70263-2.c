/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ira" } */

/* { dg-final { scan-rtl-dump "Adding REG_EQUIV to insn \[0-9\]+ for source of insn \[0-9\]+" "ira" } } */

typedef float XFtype __attribute__ ((mode (XF)));

void bar (XFtype);

void
foo (XFtype a, XFtype c)
{
  XFtype ac = a * c;

  bar (ac);
}
