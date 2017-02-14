/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE foo()
{
  if (a != 2) /* { dg-error "undeclared" } */
    goto bb1;
  else
    goto bb2;

bb1:
  a_1 = 10; /* { dg-error "undeclared" } */
  return a_1;

bb2:
  return 1;
}
