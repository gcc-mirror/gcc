/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE () foo ()
{
  int *b;
  *b = 1;
}
