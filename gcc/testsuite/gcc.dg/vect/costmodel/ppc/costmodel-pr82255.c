/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

/* PR82255: Ensure we don't require a vec_construct cost when we aren't
   going to generate a strided load.  */

extern int abs (int __x) __attribute__ ((__nothrow__, __leaf__))
__attribute__ ((__const__));

static int
foo (unsigned char *w, int i, unsigned char *x, int j)
{
  int tot = 0;
  for (int a = 0; a < 16; a++)
    {
#pragma GCC unroll 16
      for (int b = 0; b < 16; b++)
	tot += abs (w[b] - x[b]);
      w += i;
      x += j;
    }
  return tot;
}

void
bar (unsigned char *w, unsigned char *x, int i, int *result)
{
  *result = foo (w, 16, x, i);
}

/* { dg-final { scan-tree-dump-times "vec_construct" 0 "vect" } } */
