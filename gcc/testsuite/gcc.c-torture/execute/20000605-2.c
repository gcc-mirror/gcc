void abort (void);
void exit (int);

struct F { int i; };

void f1(struct F *x, struct F *y)
{
  int timeout = 0;
  for (; ((const struct F*)x)->i < y->i ; x->i++)
    if (++timeout > 5)
      abort ();
}

int
main(void)
{
  struct F x, y;
  x.i = 0;
  y.i = 1;
  f1 (&x, &y);
  exit (0);
}
