f (void (*func) ())
{
  func ();
}

main ()
{
#ifndef NO_TRAMPOLINES
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
#endif
  exit (0);
}
