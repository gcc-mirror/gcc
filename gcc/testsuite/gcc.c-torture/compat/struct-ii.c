typedef struct {int a, b;} T;

int h (T *);
T g (T);

#if COMPILER != 1
h (T *x)
{
  if (x->a != 0 || x->b != 1)
    abort ();
}
#endif

#if COMPILER != 2
T
g (T x)
{
  if (x.a != 13 || x.b != 47)
    abort ();
  x.a = 0;
  x.b = 1;
  h (&x);
  return x;
}
#endif

#if COMPILER != 1
f ()
{
  T x;
  x.a = 13;
  x.b = 47;
  g (x);
  if (x.a != 13 || x.b != 47)
    abort ();
  x = g (x);
  if (x.a != 0 || x.b != 1)
    abort ();
}
#endif

#if COMPILER != 2
main ()
{
  f ();
  exit (0);
}
#endif
