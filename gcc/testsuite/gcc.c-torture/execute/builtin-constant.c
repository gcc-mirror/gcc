/* PR optimization/8423.  */

#define btest(x) __builtin_constant_p(x) ? "1" : "0"

#ifdef __OPTIMIZE__
void
foo (char *i)
{
  if (*i == '0')
    abort ();
}
#else
void
foo (char *i)
{
}
#endif

int
main (void)
{
  int size = sizeof (int);
  foo (btest (size));
  foo (btest (size));
  exit (0);
}
