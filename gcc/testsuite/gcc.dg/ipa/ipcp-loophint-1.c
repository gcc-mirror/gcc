/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details"  } */

extern int *o, *p, *q, *r;

#define FUNCTIONS fa(), fb(), fc(), fd(), fe(), ff(), fg()

extern void FUNCTIONS;

void foo (int c)
{
  FUNCTIONS;
  FUNCTIONS;
  for (int i = 0; i < 100; i++)
    {
      for (int j = 0; j < c; j++)
	o[i] = p[i] + q[i] * r[i];
    }
  FUNCTIONS;
  FUNCTIONS;
}

void bar()
{
  foo (8);
  p[4]++;
}

/* { dg-final { scan-ipa-dump {with known iterations:[1-9]} "cp"  } } */
