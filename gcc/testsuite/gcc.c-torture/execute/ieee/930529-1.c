main ()
{
  union {
    double d;
    unsigned char c[8];
  } d;

  d.d = 1.0/7.0;

  if (sizeof (char) * 8 == sizeof (double))
    {
      if (d.c[0] == 0x92 && d.c[1] == 0x24 && d.c[2] == 0x49 && d.c[3] == 0x92
	  && d.c[4] == 0x24 && d.c[5] == 0x49 && d.c[6] == 0xc2 && d.c[7] == 0x3f)
	exit (0);
      if (d.c[7] == 0x92 && d.c[6] == 0x24 && d.c[5] == 0x49 && d.c[4] == 0x92
	  && d.c[3] == 0x24 && d.c[2] == 0x49 && d.c[1] == 0xc2 && d.c[0] == 0x3f)
	exit (0);
#if defined __arm__ || defined __thumb__
      if (d.c[4] == 0x92 && d.c[5] == 0x24 && d.c[6] == 0x49 && d.c[7] == 0x92
	  && d.c[0] == 0x24 && d.c[1] == 0x49 && d.c[2] == 0xc2 && d.c[3] == 0x3f)
	exit (0);
#endif
      abort ();
    }

  exit (0);
}
