main ()
{
  struct
    {
      signed int bf0:17;
      signed int bf1:7;
    } bf;

  bf.bf1 = 7;
  f (bf.bf1);
  exit (0);
}

f (x)
     int x;
{
  if (x != 7)
    abort ();
}
