/* PR rtl-optimization/64316 */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } } */

struct S
{
  unsigned int s;
  unsigned long w[];
};

struct S **s;

int
foo (struct S *x, struct S *y, struct S *z)
{
  unsigned int i;
  unsigned long *a, *b, *c;
  int r = 0;
  for (a = x->w, b = y->w, c = z->w, i = 0; i < x->s; i++, a++)
    {
      unsigned long d = *b++ & *c++;
      if (*a != d)
	{
	  r = 1;
	  *a = d;
	}
    }
  return r;
}

void
bar (int x)
{
  int p = x - 1;
  do
    {
      foo (s[x], s[x], s[p]);
      p--;
    }
  while (p > 0);
}
