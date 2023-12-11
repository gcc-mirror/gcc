/* { dg-do compile } */
/* { dg-options "-O -ftrivial-auto-var-init=zero" } */

int i;

void f (void)
{
  for (;;)
  {
    if (0)
      for (;;)
      {
        int *a;
        int *b = a;

 l1:
        *b = (*b != 0) ? 0 : 2;
      }

    if (i != 0)
      goto l1;
  }
}
