/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE () foo ()
{
  int _1;
  return a_1(D);  /* { dg-error "cannot have default definition" } */
}
