/* PR c/113518 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23" } */

#if __BITINT_MAXWIDTH__ >= 607
_BitInt(607) v;
#else
_BitInt(63) v;
#endif

void
foo (void)
{
  __atomic_fetch_or (&v, 1 << 31, __ATOMIC_RELAXED);
}

#if __BITINT_MAXWIDTH__ >= 16321
_BitInt(16321) w;

void
bar (void)
{
  __atomic_fetch_add (&w, 1 << 31, __ATOMIC_SEQ_CST);
}
#endif
