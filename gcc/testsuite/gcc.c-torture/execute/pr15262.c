/* We used to mis-compile this testcase as we did not know that
   &a+offsetof(b,a) was the same as &a.b */
struct A
{
  int t;
  int i;
};

void
bar (float *p)
{
  *p = 5.2;
}

int
foo(struct A *locp, int i, int str)
{
  float f, g, *p;
  int T355;
  int *T356;
  /* Currently, the alias analyzer has limited support for handling
     aliases of structure fields when no other variables are aliased.
     Introduce additional aliases to confuse it.  */
  p =  i ? &g : &f;
  bar (p);
  if (*p > 0.0)
    str = 1;

  T355 = locp->i;
  T356 = &locp->i;
  *T356 = str;
  T355 = locp->i;

  return T355;
}

main ()
{
  struct A loc;
  int str;

  loc.i = 2;
  str = foo (&loc, 10, 3);
  if (str!=1)
    abort ();
  return 0;
}

