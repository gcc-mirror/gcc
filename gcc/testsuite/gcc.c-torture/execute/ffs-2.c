struct
  {
    int input;
    int output;
  }
ffstesttab[] =
  {
#if __INT_MAX__ >= 2147483647
    /* at least 32-bit integers */
    { 0x80000000, 32 },
    { 0xa5a5a5a5, 1 },
    { 0x5a5a5a5a, 2 },
    { 0xcafe0000, 18 },
#endif
#if __INT_MAX__ >= 32767
    /* at least 16-bit integers */
    { 0x8000, 16 },
    { 0xa5a5, 1 },
    { 0x5a5a, 2 },
    { 0x0ca0, 6 },
#endif
#if __INT_MAX__ < 32767
#error integers are too small
#endif
  };

#define NFFSTESTS (sizeof (ffstesttab) / sizeof (ffstesttab[0]))

extern void abort (void);
extern void exit (int);

int
main (void)
{
  int i;

  for (i = 0; i < NFFSTESTS; i++)
    {
      if (__builtin_ffs (ffstesttab[i].input) != ffstesttab[i].output)
	abort ();
    }

  exit (0);
}
