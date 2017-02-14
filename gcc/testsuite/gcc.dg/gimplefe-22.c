/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE ()
foo (short * p)
{
  *p = _Literal (short int) 1;
  return;
}
