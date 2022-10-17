/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mno-htm" } */

/* Verify target info for inlining still works even if callee
   disables HTM explicitly while caller enables it.  */

static inline int __attribute__ ((always_inline))
foo (int *b)
{
  *b += 10;
  return *b;
}

#pragma GCC target "htm"
int
bar (int *a)
{
  *a = foo (a);
  return 0;
}

