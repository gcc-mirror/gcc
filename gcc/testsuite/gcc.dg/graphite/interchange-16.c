void spread_i1 (int *rptr, int *sptr, int ncopies, int *extent, int rdelta, int m)
{
  int *dest;
  int n;

  while (m--)
    {
      dest = rptr;
      for (n = 0; n < ncopies; n ++)
	{
	  *dest = *sptr;
	  dest += rdelta;
	}
      if (extent [n])
	if (n)
	  rptr ++;
    }
}

int main() { return 0; }

