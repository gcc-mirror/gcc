/* { dg-do run } */

extern void abort (void);

static inline void
foo (int * __restrict pr)
{
  *pr = 1;
}

int __attribute__((noinline,noclone))
bar (int * __restrict qr)
{
  *qr = 0;
  foo (qr);
  return *qr;
}

int main()
{
  int i;
  if (bar (&i) != 1)
    abort ();
  return 0;
}
