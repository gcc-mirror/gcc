/* { dg-require-effective-target trampolines } */

void exit (int);

void
f (void (*func) ())
{
  func ();
}

int
main (void)
{
  void t0 ()
    {
    }

  void t1 ()
    {
      f (t0);
    }

  void t2 ()
    {
      t1 ();
    }

  t1 ();
  t1 ();
  t2 ();

  exit (0);
}
