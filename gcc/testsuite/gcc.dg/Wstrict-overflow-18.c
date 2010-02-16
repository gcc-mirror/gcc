/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow" } */

/* Don't warn about an overflow when folding i > 0.  The loop analysis
   should determine that i does not wrap.

   The test is really bogus, p->a - p->b can be larger than INT_MAX
   and thus i can very well wrap.  */

struct c { unsigned int a; unsigned int b; };
extern void bar (struct c *);
int
foo (struct c *p)
{
  int i;
  int sum = 0;

  for (i = 0; i < p->a - p->b; ++i)
    {
      if (i > 0)  /* { dg-bogus "warning" "" { xfail *-*-* } } */
	sum += 2;
      bar (p);
    }
  return sum;
}
