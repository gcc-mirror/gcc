/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=power10" } */

extern void abort (void);

unsigned long long int
do_cfuged (unsigned long long int source, unsigned long long int mask)
{
  return __builtin_cfuged (source, mask);
}

int main (int argc, char *argv [])
{
  unsigned long long int sources [4], masks [4];
  unsigned long long int results [4][4] = {
    /* sources[0] with each of masks [0 .. 3] */
    {0x7e3ca5f0ll, 0xa5f07e3cll, 0xaf7350ecll, 0x50ecaf73ll },
    /* sources[1] with each of masks [0 .. 3] */
    { 0xa5f07e3cll,  0x7e3ca5f0ll, 0x73afec50ll, 0xec5073afll },
    /* sources[2] with each of masks [0 .. 3] */
    { 0xf07e3ca5ll, 0x3ca5f07ell, 0x3af7c50ell, 0xc50e3af7ll },
    /* sources[3] with each of masks [0 .. 3] */
    { 0xe7c35a0fll, 0x5a0fe7c3ll, 0x50ecaf73ll, 0xaf7350ecll },
  };

  sources[0] = 0xa5f07e3cll;
  sources[1] = 0x7e3ca5f0ll;
  sources[2] = 0x3ca5f07ell;
  sources[3] = 0x5a0fe7c3ll;

  masks[0] = 0xffff0000ll;
  masks[1] = 0x0000ffffll;
  masks[2] = 0x0f0f0f0fll;
  masks[3] = 0xf0f0f0f0ll;

  unsigned long long int result;

  for (int i = 0; i < 4; i++)
    {
      for (int j = 0; j < 4; j++)
	{
	  if (do_cfuged (sources[i], masks[j]) != results [i][j])
	    abort ();
	}
    }

  return 0;
}

/* { dg-final { scan-assembler {\mcfuged\M} } } */
