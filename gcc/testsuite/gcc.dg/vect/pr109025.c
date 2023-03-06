/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

int func_4(int t, int b)
{
  for (int tt1 = 0; tt1 < 128 ; tt1 ++)
    {
      for (int tt = 0; tt < 128; tt ++)
	if (b)
	  t |= 3;
      t |= 3;
    }
  return t;
}
