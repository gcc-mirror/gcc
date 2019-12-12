/* { dg-do compile { target powerpc*-*-* } }*/
/* { dg-options "-mdejagnu-cpu=power8 -O3 -fstack-protector-strong -fpic" } */

char *b;
int c, d, e, f;

void
foo (char *h, int k, int l, int m, int j, int q)
{
  char *i = b;
  int n = d;
  int s = e;
  while (c)
    {
      for (; j <= 0; j += 12)
	{
	  i[j] = n & k - h[j] >> 31 | q & ~(k - h[j] >> 31);
	  i[j + 1] = n & l - h[j + 1] >> 31 | q & ~(l - h[j + 1] >> 31);
	  i[j + 2] = n & m - h[j + 2] >> 31 | s & ~(m - h[j + 2] >> 31);
	  i[j + 3] = n & k - h[j + 3] >> 31 | q & ~(k - h[j + 3] >> 31);
	  i[j + 4] = n & l - h[j + 4] >> 31 | q & ~(l - h[j + 4] >> 31);
	  i[j + 5] = n & m - h[j + 5] >> 31 | s & ~(m - h[j + 5] >> 31);
	  i[j + 6] = n & k - h[j + 6] >> 31 | q & ~(k - h[j + 6] >> 31);
	  i[j + 7] = n & l - h[j + 7] >> 31 | q & ~(l - h[j + 7] >> 31);
	  i[j + 8] = n & m - h[j + 8] >> 31 | s & ~(m - h[j + 8] >> 31);
	  i[j + 9] = n & k - h[j + 9] >> 31 | q & ~(k - h[j + 9] >> 31);
	  i[j + 10] = n & l - h[j + 10] >> 31 | q & ~(l - h[j + 10] >> 31);
	  i[j + 11] = n & m - h[j + 11] >> 31 | s & ~(m - h[j + 11] >> 31);
	}
      while (j < f)
	;
    }
}
