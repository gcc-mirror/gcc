/* { dg-do compile } */

int sad(const unsigned char *p1, long p2)
{
  int a = 0;
  for (int y = 0; y < 16; y++)
    {
      for (int x = 0; x < 12; x++)
	a += p1[x];
      p1 += p2;
    }
  return a;
}
