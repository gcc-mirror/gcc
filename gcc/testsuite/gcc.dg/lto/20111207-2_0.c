/* { dg-lto-do run } */

int
test (void)
{
  int f (void);
  return 0;
}

int
main (void)
{
  int f (void);
  int test (void);

  return test ();
}
