#include <c6x_intrinsics.h>

extern void abort (void);

#define N 4

int vals[N] = { 0, 0xffffffff, 0x89abcdef, 0xdeadbeef };

int main ()
{
  int i;
  for (i = 0; i < N; i++)
    {
      int shf1, shf2;
      int v = vals[i];
      unsigned int uv = v;

      for (shf1 = 0; shf1 < 32; shf1++)
	for (shf2 = 0; shf2 < 32; shf2++)
	  {
	    int r = (shf1 << 5) | shf2;
	    if (shf2 > shf1)
	      {
		unsigned int mask = (1u << (shf2 - shf1) << 1) - 1;
		mask <<= shf1;
		if (_clrr (v, r) != (v & ~mask))
		  abort ();
	      }
	    if (_extr (v, r) != v << shf1 >> shf2)
	      abort ();
	    if (_extru (v, r) != uv << shf1 >> shf2)
	      abort ();
	  }
    }
  return 0;
}
