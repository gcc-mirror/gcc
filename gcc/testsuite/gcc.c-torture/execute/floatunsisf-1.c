/* The fp-bit.c function __floatunsisf had a latent bug where guard bits
   could be lost leading to incorrect rounding.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */

extern void abort (void);
extern void exit (int);
#if __INT_MAX__ >= 0x7fffffff
volatile unsigned u = 0x80000081;
#else
volatile unsigned long u = 0x80000081;
#endif
volatile float f1, f2;
int
main (void)
{
  f1 = (float) u;
  f2 = (float) 0x80000081;
  if (f1 != f2)
    abort ();
  exit (0);
}
