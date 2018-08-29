/* { dg-options "-O2" } */

struct pair
{
  double a;
  long long int b;
};

void
stp (struct pair *p)
{
  p->a = 0.0;
  p->b = 1;
}

/* { dg-final { scan-assembler "stp\txzr, x\[0-9\]+, \\\[x\[0-9\]+\\\]" } } */

void
stp2 (struct pair *p)
{
  p->a = 0.0;
  p->b = 0;
}

struct reverse_pair
{
  long long int a;
  double b;
};

void
stp_reverse (struct reverse_pair *p)
{
  p->a = 1;
  p->b = 0.0;
}

/* { dg-final { scan-assembler "stp\tx\[0-9\]+, xzr, \\\[x\[0-9\]+\\\]" } } */

void
stp_reverse2 (struct reverse_pair *p)
{
  p->a = 0;
  p->b = 0.0;
}

/* { dg-final { scan-assembler-times "stp\txzr, xzr, \\\[x\[0-9\]+\\\]" 2 } } */
