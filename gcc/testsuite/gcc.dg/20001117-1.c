/* { dg-do run } */
/* { dg-options "-O2 -finstrument-functions" } */
/* { dg-require-effective-target return_address } */

extern void abort (void);
extern void exit (int);

double
foo (double a, double b)
{
  double c;
  if (0.0 < a)
    c = a;
  else if (b > 0.0)
    c = 0.0;
  else
    return 0;
  return 2.0 * (b - c);
}

int main ()
{
  if (foo (1.0, 4.0) != 6.0)
    abort ();
  exit (0);
}

/* Abort on non-NULL CALL_SITE to ensure that __builtin_return_address
   was expanded properly.  */
void __attribute__((no_instrument_function))
__cyg_profile_func_enter(void *this_fn, void *call_site)
{
  if (call_site == (void *)0)
    abort ();
}

void __attribute__((no_instrument_function))
__cyg_profile_func_exit(void *this_fn, void *call_site)
{
  if (call_site == (void *)0)
    abort ();
}

