extern inline int foo (void)
{
  return 23;
}
int bar (void)
{
  return foo ();
}
extern int foo (void) __attribute__ ((weak, alias ("xxx")));
int baz (void)
{
  return foo ();
}
