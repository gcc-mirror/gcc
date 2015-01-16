/* { dg-do compile } */
/* { dg-options "-O" } */

extern int a[];

void loop_clear (int i)
{
  while (i > 0)
    a[i--] = 0;
}

/* { dg-final { scan-assembler-times "cmp" 1 { xfail *-*-* } } } */

/* FIXME: the redundant cmp is not eliminated because the compare-elim pass
   is run before the dbr pass.  It's a regression wrt the cc0 port.  */
