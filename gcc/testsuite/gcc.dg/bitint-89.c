/* PR tree-optimization/113818 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-Os -fnon-call-exceptions -finstrument-functions-once" } */

int c, i;
void bar (int *);

#if __BITINT_MAXWIDTH__ >= 129
_BitInt(129) *a;
#else
_BitInt(63) *a;
#endif

void
foo (void)
{
  if (c)
    return;
  int q;
  a[i] = 0;
  bar (&q);
}
