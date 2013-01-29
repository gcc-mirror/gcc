/* PR rtl-optimization/56117 */
/* { dg-do compile } */
/* { dg-options "-O2 -fsched2-use-superblocks" } */

void
foo (void *p)
{
  __builtin_prefetch (p);
}
