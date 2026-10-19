/* PR rtl-optimization/127796 */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-var-tracking -fcompare-debug" } */

/* Keep debug markers as DEBUG_INSNs through delay-slot reorg.  */

/* Taken from gcc.c-torture/compile/pr110705-1.c.  */

struct a {
  long b;
};
union d {
  struct a b;
  int e;
} v;
long c;
int f;

static void
g (union d h, long i)
{
  while (1)
    switch (c)
    case 4:
      if (h.e)
	c = 4;
}

void
j (union d *h)
{
  if (f)
    g (*h, h->b.b);
}

void
k ()
{
  union d *h = &v;
  j (h);
}
