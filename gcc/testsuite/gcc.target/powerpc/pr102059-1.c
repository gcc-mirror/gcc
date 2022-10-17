/* { dg-do compile } */
/* -Wno-attributes suppresses always_inline warnings.  */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -Wno-attributes" } */

/* Verify the reduced case from PR102059 won't fail.  */

__attribute__ ((always_inline)) int
foo (int *b)
{
  *b += 10;
  return *b;
}

#pragma GCC target "cpu=power10"
int
bar (int *a)
{
  *a = foo (a);
  return 0;
}

