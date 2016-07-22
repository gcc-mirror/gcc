// PR c++/70744
// { dg-do run }
// { dg-options "" }

static void
fn1 (void)
{
  int x = 2;
  ++x ? : 42;
  if (x != 3)
    __builtin_abort ();
  --x ? : 42;
  if (x != 2)
    __builtin_abort ();
  x++ ? : 42;
  if (x != 3)
    __builtin_abort ();
  x-- ? : 42;
  if (x != 2)
    __builtin_abort ();
}

int
main ()
{
  fn1 ();
  return 0;
}
