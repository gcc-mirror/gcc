/* { dg-require-stack-size "2055*3*8" } */

f ()
{
  long dx[2055];
  long dy[2055];
  long s1[2055];
  int x, y;
  int i;
  long s;

  for (;;)
    {
      s = 2055;
      g (s1, s);
      for (i = 0; i < 1; i++);
      dy[s] = 0x12345;
      for (i = 0; i < 1; i++);
      if (x != y || h (dx, dy, s) || dx[s] != 0x12345)
	{
	  j (y);k (dy);
	}
    }
}
