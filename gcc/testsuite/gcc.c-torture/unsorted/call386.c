
foo () {}

main ()
{
  int i;
  for (i = 100000; i >= 0; i--)
    {
      foo ();
      foo ();
      foo ();
      foo ();
      foo ();
      foo ();
      foo ();
      foo ();
      foo ();
      foo ();
    }
}
