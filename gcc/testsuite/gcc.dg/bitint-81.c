/* PR middle-end/113699 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23" } */

void
foo (void)
{
#if __BITINT_MAXWIDTH__ >= 129
  _BitInt(129) i;
  __asm__ ("" : : "rm" (i));
#endif
}
