/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE () foo ()
{
  int a;
  char b;
  a = (int) b;
  return;
}
