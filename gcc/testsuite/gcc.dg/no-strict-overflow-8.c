/* { dg-do compile } */
/* { dg-options "-fno-strict-overflow -O2 -fdump-tree-optimized" } */

/* We cannot fold i > 0 because p->a - p->b can be larger than INT_MAX
   and thus i can wrap.  Dual of Wstrict-overflow-18.c  */

struct c { unsigned int a; unsigned int b; };
extern void bar (struct c *);
int
foo (struct c *p)
{
  int i;
  int sum = 0;

  for (i = 0; i < p->a - p->b; ++i)
    {
      if (i > 0)
	sum += 2;
      bar (p);
    }
  return sum;
}

/* { dg-final { scan-tree-dump "i_.* > 0" "optimized" } } */
