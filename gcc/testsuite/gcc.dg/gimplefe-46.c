/* { dg-do compile } */
/* { dg-options "-fgimple" } */

char global[10];

void bar (void);

void __GIMPLE (ssa)
foo (char * p)
{
  __BB(2):
  if (p_2(D) == _Literal (char *)&global[2])
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  bar ();
  goto __BB4;

  __BB(4):
  return;
}
