/* PR tree-optimization/113691 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=gnu11 -w" } */

#if __BITINT_MAXWIDTH__ >= 944
_BitInt (944) i;
#else
_BitInt (63) i;
#endif

void foo ();

void
bar ()
{
  foo (i);
}

void
foo (int *p)
{
  *p = 0;
}
