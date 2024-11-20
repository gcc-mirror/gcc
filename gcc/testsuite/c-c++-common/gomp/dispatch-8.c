int g1() { return 100; }
int g2() { return 200; }

#pragma omp declare variant (g1) match (construct={dispatch},user={condition(1)})
#pragma omp declare variant (g2) match (user={condition(1)})
int g() { return 300; }


int f1(int x) { return 1 + x; }
int f2(int x) { return 2 + x; }

#pragma omp declare variant (f1) match (construct={dispatch},user={condition(1)})
#pragma omp declare variant (f2) match (user={condition(1)})
int f(int x) { return 3 + x; }


void
test ()
{
  int res;

  // Call f2(g2()) due to user condition(1)
  res = f (g ());
  __builtin_printf("%d\n", res);
  if (res != 202)
    __builtin_abort ();

  // Call 'f1(g1())' due to construct 'dispatch' and user conditional(1)
  #pragma omp dispatch
    res = f (g ());
  __builtin_printf("%d\n", res);
  if (res != 101)
     __builtin_abort ();

  // Call 'f2(g2())' due to nocontext (i.e. ignore construct 'dispatch') and user conditional(1)
  #pragma omp dispatch nocontext(1)
    res = f (g ());
  __builtin_printf("%d\n", res);
  if (res != 202)
     __builtin_abort ();

  // Call 'f' due to 'novariants'
  // Call 'g1' due to construct 'dispatch' and user conditional(1)
  #pragma omp dispatch novariants(1)
    res = f (g ());
  __builtin_printf("%d\n", res);   /* ACTUAL RESULT: 303, i.e. 'g' and not 'g1' was called */
  if (res != 103)
     __builtin_abort ();

  // Call 'f' due to 'novariants'
  // Call 'g2' due to  nocontext (i.e. ignore construct 'dispatch') and user conditional(1)
  #pragma omp dispatch novariants(1) nocontext(1)
    res = f (g ());
  __builtin_printf("%d\n", res);   /* ACTUAL RESULT: 303, i.e. 'g' and not 'g2' was called */
  if (res != 203)
     __builtin_abort ();
}

int
main ()
{
  test ();
}
