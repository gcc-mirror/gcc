/* { dg-do compile }*/
/* { dg-options "-fgimple" } */

int a;
void __GIMPLE () foo ()
{
  int b;
  b = a;
  b = b + 1;
  a = b;
}
