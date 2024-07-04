void abort (void);
void exit (int);

union u {
  struct { int i1, i2; } t;
  double d;
} x[2], v;

void
f (x, v)
     union u *x, v;
{
  *++x = v;
}

int
main(void)
{
  x[1].t.i1 = x[1].t.i2 = 0;
  v.t.i1 = 1;
  v.t.i2 = 2;
  f (x, v);
  if (x[1].t.i1 != 1 || x[1].t.i2 != 2)
    abort ();
  exit (0);
}
