dd (x,d) { return x / d; }

main ()
{
  int i;
  for (i = -3; i <= 3; i++)
    {
      if (dd (i, 1) != i / 1)
	abort ();
      if (dd (i, 2) != i / 2)
	abort ();
      if (dd (i, 3) != i / 3)
	abort ();
      if (dd (i, 4) != i / 4)
	abort ();
      if (dd (i, 5) != i / 5)
	abort ();
      if (dd (i, 6) != i / 6)
	abort ();
      if (dd (i, 7) != i / 7)
	abort ();
      if (dd (i, 8) != i / 8)
	abort ();
    }
  for (i = ((unsigned) ~0 >> 1) - 3; i <= ((unsigned) ~0 >> 1) + 3; i++)
    {
      if (dd (i, 1) != i / 1)
	abort ();
      if (dd (i, 2) != i / 2)
	abort ();
      if (dd (i, 3) != i / 3)
	abort ();
      if (dd (i, 4) != i / 4)
	abort ();
      if (dd (i, 5) != i / 5)
	abort ();
      if (dd (i, 6) != i / 6)
	abort ();
      if (dd (i, 7) != i / 7)
	abort ();
      if (dd (i, 8) != i / 8)
	abort ();
    }
  exit (0);
}
