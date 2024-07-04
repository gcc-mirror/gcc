/* PR 15262.  Similar to pr15262-1.c but with no obvious addresses
   being taken in function foo().  Without IPA, by only looking inside
   foo() we cannot tell for certain whether 'q' and 'b' alias each
   other.  */

void abort (void);

struct A
{
  int t;
  int i;
};

struct B
{
  int *p;
  float b;
};

float X;

int
foo (struct B b, struct A *q, float *h)
{
  X += *h;
  *(b.p) = 3;
  q->t = 2;
  return *(b.p);
}

int
main(void)
{
  struct A a;
  struct B b;

  b.p = &a.t;
  if (foo (b, &a, &X) == 3)
    abort ();

  return 0;
}
