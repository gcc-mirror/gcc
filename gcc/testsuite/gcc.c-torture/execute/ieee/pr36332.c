/* PR target/36332 */

int
foo (long double ld)
{
  return ld == __builtin_infl ();
}

int
main ()
{
  if (foo (__LDBL_MAX__))
    __builtin_abort ();
  return 0;
}
