/* PR middle-end/122773 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough -O2 -ftrivial-auto-var-init=zero" } */

void *l;
int
foo (int x)
{
  __label__ l1, l2, l3;
  static void *l[] = { &&l1, &&l2, &&l3 };
  switch (0)
    {
    case 0:
      while (0)
        ;
      goto *l[x];
    }
  l1:
  ++x;
  l2:
  ++x;
  l3:
  ++x;
  return x;
}
