/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23 -O2" } */

/* Test _BitInt alignment for PowerPC.  Widths up to 32 bits use QImode,
   HImode or SImode.  Anything wider is an array of limbs -- DImode on
   ppc64, SImode on ppc32 -- and so takes the alignment of a limb,
   regardless of endianness.  */

#ifdef __powerpc64__
#define LIMB_ALIGNMENT 8
#else
#define LIMB_ALIGNMENT 4
#endif

int
main (void)
{
  if (__alignof__ (_BitInt(8)) != 1)
    __builtin_abort ();

  if (__alignof__ (_BitInt(16)) != 2)
    __builtin_abort ();

  if (__alignof__ (_BitInt(32)) != 4)
    __builtin_abort ();

  if (__alignof__ (_BitInt(33)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  if (__alignof__ (_BitInt(64)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  if (__alignof__ (_BitInt(65)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  if (__alignof__ (_BitInt(128)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  if (__alignof__ (_BitInt(129)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  if (__alignof__ (_BitInt(200)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  if (__alignof__ (_BitInt(256)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  /* Unsigned variants have the same alignment.  */
  if (__alignof__ (unsigned _BitInt(8)) != 1)
    __builtin_abort ();

  if (__alignof__ (unsigned _BitInt(16)) != 2)
    __builtin_abort ();

  if (__alignof__ (unsigned _BitInt(32)) != 4)
    __builtin_abort ();

  if (__alignof__ (unsigned _BitInt(64)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  if (__alignof__ (unsigned _BitInt(128)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  if (__alignof__ (unsigned _BitInt(256)) != LIMB_ALIGNMENT)
    __builtin_abort ();

  return 0;
}
