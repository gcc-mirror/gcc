/* PR debug/113637 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-g -std=c23" } */

#if __BITINT_MAXWIDTH__ >= 639
typedef _BitInt(639) B;
#else
typedef _BitInt(63) B;
#endif

void
foo (B n)
{
  extern void bar (int [][n]);
}
