/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mno-power8-fusion -fdump-tree-einline-optimized" } */

/* Like pr102059-1.c, to verify the inlining still happens
   even without always_inline attribute.  */

int foo (int *b)
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

/* { dg-final { scan-tree-dump-times {Inlining foo/[0-9]* } 1 "einline"} } */
