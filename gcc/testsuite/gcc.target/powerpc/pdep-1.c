/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=power10" } */

extern void abort (void);

unsigned long long int
do_pdepd (unsigned long long int source, unsigned long long int mask) {
  return __builtin_pdepd (source, mask);
}

int main (int argc, char *argv [])
{
  unsigned long long int sources [4], masks [4];
  unsigned long long int results [4][4] = {
    /* sources [0] with each of masks [0..3] */
    { 0x7e3c0000ll, 0x00007e3cll, 0x070e030cll, 0x70e030c0ll },
    /* sources [1] with each of masks [0..3] */
    { 0xa5f00000ll, 0x0000a5f0ll, 0x0a050f00ll, 0xa050f000ll },
    /* sources [2] with each of masks [0..3] */
    { 0xf07e0000ll, 0x0000f07ell, 0x0f00070ell, 0xf00070e0ll },
    /* sources [3] with each of masks [0..3] */
    { 0xe7c30000ll, 0x0000e7c3ll, 0x0e070c03ll, 0xe070c030ll },
  };

  sources[0] = 0xa5f07e3cll;
  sources[1] = 0x7e3ca5f0ll;
  sources[2] = 0x3ca5f07ell;
  sources[3] = 0x5a0fe7c3ll;

  masks[0] = 0xffff0000ll;
  masks[1] = 0x0000ffffll;
  masks[2] = 0x0f0f0f0fll;
  masks[3] = 0xf0f0f0f0ll;

  for (int i = 0; i < 4; i++)
    {
      for (int j = 0; j < 4; j++)
	{
	  if (do_pdepd (sources[i], masks[j]) != results [i][j])
	    abort ();
	}
    }

  return 0;
}

