/* PR middle-end/113410 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23" } */

#if __BITINT_MAXWIDTH__ >= 905
void bar (_BitInt(905) n, int[n]);
#else
void bar (int n, int[n]);
#endif

void
foo (int n)
{
  int buf[n];
  bar (n, buf);
}
