/* PR tree-optimization/113463 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

extern char *a, *b;
#if __BITINT_MAXWIDTH__ >= 129
_BitInt(129) o;
#else
_BitInt(63) o;
#endif

void
foo (void)
{
  __builtin_memcpy (a + o, b, 4);
}
