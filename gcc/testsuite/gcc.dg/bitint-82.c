/* PR tree-optimization/113692 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23" } */

#if __BITINT_MAXWIDTH__ >= 135
_BitInt(135) i;
#else
_BitInt(63) i;
#endif

void *
foo (void)
{
  void *ret = 0;
  if (i & 1)
    ret = (void *) 1;
  return ret;
}
