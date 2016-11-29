/* { dg-do compile } */
/* { dg-options "-O -fgimple" } */

void __GIMPLE (startwith ("ccp1")) foo ()
{
  int a;
  int b;
  a = b + 2;
  return;
}
