extern void exit (int);
extern void abort (void);

volatile int i;
unsigned char *volatile cp;
unsigned char d[32] = { 0 };

int
main (void)
{
  unsigned char c[32] = { 0 };
  unsigned char *p = d + i;
  int j;
  for (j = 0; j < 30; j++)
    {
      int x = 0xff;
      int y = *++p;
      switch (j)
	{
	case 1: x ^= 2; break;
	case 2: x ^= 4; break;
	case 25: x ^= 1; break;
	default: break;
	}
      c[j] = y | x;
      cp = p;
    }
  if (c[0] != 0xff
      || c[1] != 0xfd
      || c[2] != 0xfb
      || c[3] != 0xff
      || c[4] != 0xff
      || c[25] != 0xfe
      || cp != d + 30)
    abort ();
  exit (0);
}
