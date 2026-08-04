/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_hw } */
/* { dg-options "-O3 -march=armv8.2-a+dotprod -mautovec-preference=asimd-only" } */

/* Both expansions of a widening sum reduction, with and without dot
   product, must agree with a scalar sum for every narrow to wide type
   pair.  The accumulators are unsigned so that overflow wraps.  */

#define TYPES(X)				\
  X (u8_h,  unsigned char,  unsigned short)	\
  X (i8_h,  signed char,    unsigned short)	\
  X (u8_i,  unsigned char,  unsigned int)	\
  X (i8_i,  signed char,    unsigned int)	\
  X (u8_l,  unsigned char,  unsigned long)	\
  X (i8_l,  signed char,    unsigned long)	\
  X (u16_i, unsigned short, unsigned int)	\
  X (i16_i, short,          unsigned int)	\
  X (u16_l, unsigned short, unsigned long)	\
  X (i16_l, short,          unsigned long)	\
  X (u32_l, unsigned int,   unsigned long)	\
  X (i32_l, int,            unsigned long)

#define SUM(PREFIX, NAME, ITYPE, OTYPE)				\
  __attribute__ ((noipa))					\
  OTYPE PREFIX##_##NAME (const ITYPE *a, int n)			\
  {								\
    OTYPE s = 0;						\
    for (int i = 0; i < n; i++)					\
      s += a[i];						\
    return s;							\
  }

#define DOT(NAME, ITYPE, OTYPE) SUM (dot, NAME, ITYPE, OTYPE)
#define NODOT(NAME, ITYPE, OTYPE) SUM (nodot, NAME, ITYPE, OTYPE)

/* A volatile accumulator keeps this loop scalar.  */
#define REF(NAME, ITYPE, OTYPE)					\
  __attribute__ ((noipa))					\
  OTYPE ref_##NAME (const ITYPE *a, int n)			\
  {								\
    volatile OTYPE s = 0;					\
    for (int i = 0; i < n; i++)					\
      s = s + a[i];						\
    return s;							\
  }

TYPES (DOT)
TYPES (REF)

#pragma GCC push_options
#pragma GCC target ("+nodotprod")
TYPES (NODOT)
#pragma GCC pop_options

#define BYTES 8192
static unsigned char buf[BYTES] __attribute__ ((aligned (64)));

#define CHECK(NAME, ITYPE, OTYPE)					\
  {									\
    const ITYPE *p = (const ITYPE *) (buf + off);			\
    OTYPE want = ref_##NAME (p, n);					\
    if (dot_##NAME (p, n) != want || nodot_##NAME (p, n) != want)	\
      __builtin_abort ();						\
  }

int
main (void)
{
  unsigned long x = 1;
  for (int i = 0; i < BYTES; i++)
    {
      x = x * 6364136223846793005UL + 1442695040888963407UL;
      buf[i] = x >> 40;
    }

  for (int off = 0; off < 8; off += 4)
    for (int n = 0; n <= 260; n++)
      {
	TYPES (CHECK)
      }

  return 0;
}
