main ()
{
  long dx[2055];
  long dy[2055];
  long s1[2055];
  int cyx, cyy;
  int i;
  long size;

  for (;;)
    {
      size = 2055;
      mpn_random2 (s1, size);

      for (i = 0; i < 1; i++)
	;

      dy[size] = 0x12345678;

      for (i = 0; i < 1; i++)
	cyy = mpn_mul_1 (dy, s1, size);

      if (cyx != cyy || mpn_cmp (dx, dy, size + 1) != 0 || dx[size] != 0x12345678)
	{
	  foo ("", 8, cyy); mpn_print (dy, size);
	}
      exxit();
    }
}

foo (){}
mpn_mul_1(){}
mpn_print (){}
mpn_random2(){}
mpn_cmp(){}
exxit(){exit(0);}
