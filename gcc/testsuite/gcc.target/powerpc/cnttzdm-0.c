/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=power10" } */

extern void abort (void);

unsigned long long int
do_cnttzdm (unsigned long long int source, unsigned long long int mask) {
  return __builtin_cnttzdm (source, mask);
}

int main (int argc, char *argv [])
{
  unsigned long long int sources [4], masks [4];
  unsigned long long int intermediates [4][4] = {
    /* sources[0] with each of masks [0 .. 3] */
    { 0x0000a5f0ll, 0x00007e3cll, 0x000050ecll, 0x0000af73ll },
    /* sources[1] with each of masks [0 .. 3] */
    { 0x00007e3cll, 0x0000a5f0ll, 0x0000ec50ll, 0x000073afll },
    /* sources[2] with each of masks [0 .. 3] */
    { 0x00003ca5ll, 0x0000f07ell, 0x0000c50ell, 0x00003af7ll },
    /* sources[3] with each of masks [0 .. 3] */
    { 0x00005a0fll, 0x0000e7c3ll, 0x0000af73ll, 0x000050ecll },
  };
  unsigned long long int results [4][4] = {
    { 4, 2, 2, 0 },
    { 2, 4, 4, 0 },
    { 0, 1, 1, 0 },
    { 0, 0, 0, 2 },
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
	  if (do_cnttzdm (sources[i], masks[j]) != results [i][j])
	    abort ();
	}
    }

  return 0;
}

/* { dg-final { scan-assembler {\mcnttzdm\M} } } */
