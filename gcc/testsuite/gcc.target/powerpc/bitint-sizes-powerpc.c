/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23 -O0" } */

/* Test _BitInt sizes for PowerPC.  Widths up to 32 bits use QImode,
   HImode or SImode.  Anything wider occupies a whole number of limbs --
   DImode on ppc64, SImode on ppc32 -- so the size is
   ceil (n / LIMB_BITS) * LIMB_BYTES, regardless of endianness.  */

#ifdef __powerpc64__
#define LIMB_BYTES 8
#else
#define LIMB_BYTES 4
#endif

#define LIMB_BITS (LIMB_BYTES * __CHAR_BIT__)

static long unsigned int
calc_size (int n)
{
  if (n > 32)
    return ((n + LIMB_BITS - 1) / LIMB_BITS) * LIMB_BYTES;
  if (n > 16)
    return sizeof (int);
  if (n > 8)
    return sizeof (short);
  return sizeof (char);
}

#define CHECK_SIZE(N) \
  if (sizeof (_BitInt(N)) != calc_size (N)) \
    __builtin_abort ()

#define CHECK_USIZE(N) \
  if (sizeof (unsigned _BitInt(N)) != calc_size (N)) \
    __builtin_abort ()

int
main (void)
{
  /* Small sizes.  */
  CHECK_USIZE(1);
  CHECK_SIZE(2);
  CHECK_SIZE(3);
  CHECK_SIZE(7);
  CHECK_SIZE(8);

  /* 9-16 bits.  */
  CHECK_SIZE(9);
  CHECK_SIZE(13);
  CHECK_SIZE(15);
  CHECK_SIZE(16);

  /* 17-32 bits.  */
  CHECK_SIZE(17);
  CHECK_SIZE(24);
  CHECK_SIZE(31);
  CHECK_SIZE(32);

  /* 33-64 bits.  */
  CHECK_SIZE(33);
  CHECK_SIZE(42);
  CHECK_SIZE(53);
  CHECK_SIZE(63);
  CHECK_SIZE(64);

  /* 65-128 bits.  */
  CHECK_SIZE(65);
  CHECK_SIZE(79);
  CHECK_SIZE(96);
  CHECK_SIZE(113);
  CHECK_SIZE(127);
  CHECK_SIZE(128);

  /* Larger.  */
  CHECK_SIZE(129);
  CHECK_SIZE(153);
  CHECK_SIZE(255);
  CHECK_SIZE(256);
  CHECK_SIZE(257);
  CHECK_SIZE(353);
  CHECK_SIZE(512);
  CHECK_SIZE(620);
  CHECK_SIZE(1024);
  CHECK_SIZE(2048);

  return 0;
}
