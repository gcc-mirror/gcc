/* PR tree-optimization/113567 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 129
_BitInt(129) v;

void
foo (_BitInt(129) a, int i)
{
  __label__  l1, l2;
  i &= 1;
  void *p[] = { &&l1, &&l2 };
l1:
  a %= 3;
  v = a;
  i = !i;
  goto *(p[i]);
l2:;
}
#else
int i;
#endif
