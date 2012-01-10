/* { dg-lto-do run } */
/* { dg-lto-options { { -g -O -flto } } } */

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
