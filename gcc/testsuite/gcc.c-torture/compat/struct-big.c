typedef struct {int a, b, c, d, e;} T;

int h (T *);
T g (T);

#if COMPILER != 1
h (T *x)
{
  if (x->a != 0 || x->b != 1 || x->c != 2 || x->d != 3 || x->e != 4)
    abort ();
}
#endif

#if COMPILER != 2
T
g (T x)
{
  if (x.a != 13 || x.b != 47 || x.c != 123456 || x.d != -4711 || x.e != -2)
    abort ();
  x.a = 0;
  x.b = 1;
  x.c = 2;
  x.d = 3;
  x.e = 4;
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
  x.c = 123456;
  x.d = -4711;
  x.e = -2;
  g (x);
  if (x.a != 13 || x.b != 47 || x.c != 123456 || x.d != -4711 || x.e != -2)
    abort ();
  x = g (x);
  if (x.a != 0 || x.b != 1 || x.c != 2 || x.d != 3 || x.e != 4)
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
