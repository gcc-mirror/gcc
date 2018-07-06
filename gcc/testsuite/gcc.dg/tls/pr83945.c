/* PR middle-end/83945 */
/* { dg-do compile { target tls } } */
/* { dg-options "-O2" } */

struct S { int a[1]; };
__thread struct T { int c; } e;
int f;
void bar (int);

void
foo (int f, int x)
{
  struct S *h = (struct S *) &e.c;
  for (;;)
    {
      int *a = h->a, i;
      for (i = x; i; i--)
	bar (a[f]);
      bar (a[f]);
    }
}
