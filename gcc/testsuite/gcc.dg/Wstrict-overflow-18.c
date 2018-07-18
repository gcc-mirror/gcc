/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow" } */

/* Warn about an overflow when folding i > 0, p->a - p->b can be larger
   than INT_MAX and thus i can wrap.  */

struct c { unsigned int a; unsigned int b; };
extern void bar (struct c *);
int
foo (struct c *p)
{
  int i;
  int sum = 0;

  for (i = 0; i < p->a - p->b; ++i)
    {
      /* See PR80511 for the XFAIL.  */
      if (i > 0)  /* { dg-warning "signed overflow" "" { xfail *-*-* } } */
	sum += 2;
      bar (p);
    }
  return sum;
}
