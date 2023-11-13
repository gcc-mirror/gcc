void abort (void);
void exit (int);

struct S
{
  int *sp, fc, *sc, a[2];
};

void
f (struct S *x)
{
  int *t = x->sc;
  int t1 = t[0];
  int t2 = t[1];
  int t3 = t[2];
  int a0 = x->a[0];
  int a1 = x->a[1];
  asm("": :"r" (t2), "r" (t3));
  t[2] = t1;
  t[0] = a1;
  x->a[1] = a0;
  x->a[0] = t3;
  x->fc = t2;
  x->sp = t;
}

int
main (void)
{
  struct S s;
  static int sc[3] = {2, 3, 4};
  s.sc = sc;
  s.a[0] = 10;
  s.a[1] = 11;
  f (&s);
  if (s.sp[2] != 2)
    abort ();
  exit (0);
}
