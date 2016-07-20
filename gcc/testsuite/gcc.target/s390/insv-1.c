/* { dg-do compile } */
/* { dg-options "-O3 -march=z10 -mzarch" } */

unsigned long
foo1 (unsigned long a, unsigned long b)
{
  return (a << 5) | (b & (((1UL << 5) - 1)));
}

/* This generates very different RTX than foo1.  The output reg (r2)
   matches the unshifted argument.  So it actually is a
   (set (zero_extract a 59 0) b) */
unsigned long
foo2 (unsigned long a, unsigned long b)
{
  return (b << 5) | (a & (((1UL << 5) - 1)));
}

/* risbg cannot be used when less bits are removed with the mask.  */

unsigned long
foo1b (unsigned long a, unsigned long b)
{
  return (a << 5) | (b & 1);
}

unsigned long
foo2b (unsigned long a, unsigned long b)
{
  return (b << 5) | (a & 1);
}

/* risbg cannot be used when the masked bits would end up in the
   result since a real OR is required then.  */
unsigned long
foo1c (unsigned long a, unsigned long b)
{
  return (a << 5) | (b & 127);
}

unsigned long
foo2c (unsigned long a, unsigned long b)
{
  return (b << 5) | (a & 127);
}

unsigned long
foo3 (unsigned long a, unsigned long b)
{
#ifdef __s390x__
  return (a << 5) | (b >> 59);
#else
  return (a << 5) | (b >> 27);
#endif
}

unsigned long
foo4 (unsigned long a, unsigned long b)
{
#ifdef __s390x__
  return (b << 5) | (a >> 59);
#else
  return (b << 5) | (a >> 27);
#endif
}

/* risbg can be used also if there are some bits spared in the middle
   of the two chunks.  */
unsigned long
foo3b (unsigned long a, unsigned long b)
{
#ifdef __s390x__
  return (a << 6) | (b >> 59);
#else
  return (a << 6) | (b >> 27);
#endif
}

unsigned long
foo4b (unsigned long a, unsigned long b)
{
#ifdef __s390x__
  return (b << 6) | (a >> 59);
#else
  return (b << 6) | (a >> 27);
#endif
}

/* One bit of overlap so better don't use risbg.  */

unsigned long
foo3c (unsigned long a, unsigned long b)
{
#ifdef __s390x__
  return (a << 4) | (b >> 59);
#else
  return (a << 4) | (b >> 27);
#endif
}

unsigned long
foo4c (unsigned long a, unsigned long b)
{
#ifdef __s390x__
  return (b << 4) | (a >> 59);
#else
  return (b << 4) | (a >> 27);
#endif
}

/* The functions foo3, foo4, foo3b, foo4b no longer use risbg but rosbg instead.

   On s390x, four risbg go away and four new ones appear in other functions ...
     { dg-final { scan-assembler-times "risbg" 6 { target { s390x-*-* } } } }

   but not on s390.
     { dg-final { scan-assembler-times "risbg" 2 { target { s390-*-* } } } }
*/
