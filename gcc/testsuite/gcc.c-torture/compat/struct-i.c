typedef struct {int a;} T;

int h (T *);
T g (T);

#if COMPILER != 1
h (T *x)
{
  if (x->a != 47114711)
    abort ();
}
#endif

#if COMPILER != 2
T
g (T x)
{
  if (x.a != 13)
    abort ();
  x.a = 47114711;
  h (&x);
  return x;
}
#endif

#if COMPILER != 1
f ()
{
  T x;
  x.a = 13;
  g (x);
  if (x.a != 13)
    abort ();
  x = g (x);
  if (x.a != 47114711)
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
