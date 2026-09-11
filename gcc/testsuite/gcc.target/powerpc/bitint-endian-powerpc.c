/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23 -O0" } */

/* Test the in-memory representation of multi-limb _BitInt on PowerPC.

   A _BitInt(N) is stored as an array of limbs: DImode (8 byte) limbs on
   64-bit PowerPC and SImode (4 byte) limbs on 32-bit PowerPC.  The
   object therefore occupies ceil(N / limb_bits) limbs, which is more
   than ceil(N / 8) bytes whenever N is not a multiple of the limb
   width, and the value is right justified inside it.

   The position of that slack is what differs between endiannesses, and
   is the thing worth testing:

     - big-endian    the most significant limb comes first, so the
		     padding is at the *start* of the object and the
		     value begins at byte (sizeof - ceil (N / 8));
     - little-endian the least significant limb comes first, so the
		     value begins at byte 0 and the padding is at the
		     *end*.

   Reading the first ceil (N / 8) bytes therefore works by accident on
   little-endian and is wrong on big-endian.

   Padding bits are unspecified for this target (bitint_ext_undef), so
   this test never examines them; where a value byte is only partly
   covered by the precision, only the value bits in it are compared.  */

/* The limb is the widest integer that fits in one general purpose
   register, which is exactly what unsigned long is on all three
   supported PowerPC ABIs.  */
#define LIMB_BYTES ((int) sizeof (unsigned long))

/* Check that OBJ, an ABI object holding a PREC-bit _BitInt, stores the
   value whose big-endian byte representation is VAL[0 .. (PREC+7)/8-1]
   (most significant byte first).  */

static void
check_layout (const void *obj, int prec, const unsigned char *val)
{
  const unsigned char *b = (const unsigned char *) obj;
  int limb_bits = LIMB_BYTES * 8;
  int obj_bytes = ((prec + limb_bits - 1) / limb_bits) * LIMB_BYTES;
  int val_bytes = (prec + 7) / 8;
  int pad_bytes = obj_bytes - val_bytes;
  /* Bits of the most significant value byte that belong to the value;
     the rest of that byte, if any, is padding.  */
  int top_bits = prec % 8;
  unsigned char top_mask
    = top_bits ? (unsigned char) ((1u << top_bits) - 1) : (unsigned char) 0xff;

  for (int i = 0; i < val_bytes; i++)
    {
      /* I counts value bytes down from the most significant one.  */
      int idx;
      unsigned char got, want;

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
      idx = pad_bytes + i;
#else
      idx = val_bytes - 1 - i;
#endif
      got = b[idx];
      want = val[i];
      if (i == 0)
	{
	  got &= top_mask;
	  want &= top_mask;
	}
      if (got != want)
	__builtin_abort ();
    }
}

int
main (void)
{
  /* Exact multiple of 8 bits, but not of either limb width: 12 value
     bytes in a 16 byte object on ppc64, in a 12 byte object on ppc32.  */
  {
    _BitInt(96) val = 0x010203040506070809101112wb;
    static const unsigned char expected[12]
      = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
	  0x07, 0x08, 0x09, 0x10, 0x11, 0x12 };

    check_layout (&val, 96, expected);
  }

  /* Not a multiple of 8 bits: the most significant value byte holds a
     single value bit, the other seven are padding.  */
  {
    unsigned _BitInt(65) val
      = ((unsigned _BitInt(65)) 0x01u << 64)
      | (unsigned _BitInt(65)) 0x02030405u;
    static const unsigned char expected[9]
      = { 0x01, 0x00, 0x00, 0x00, 0x00, 0x02, 0x03, 0x04, 0x05 };

    check_layout (&val, 65, expected);
  }

  /* Several limbs, to check limb ordering rather than just the ends:
     20 value bytes in a 24 byte object on ppc64, in a 20 byte object
     on ppc32.  */
  {
    unsigned _BitInt(160) value
      = ((unsigned _BitInt(160)) 0x01020304u << 128)
      | ((unsigned _BitInt(160)) 0x11121314u << 96)
      | ((unsigned _BitInt(160)) 0x21222324u << 64)
      | ((unsigned _BitInt(160)) 0x31323334u << 32)
      | (unsigned _BitInt(160)) 0x41424344u;
    static const unsigned char expected[20]
      = { 0x01, 0x02, 0x03, 0x04, 0x11, 0x12, 0x13, 0x14,
	  0x21, 0x22, 0x23, 0x24, 0x31, 0x32, 0x33, 0x34,
	  0x41, 0x42, 0x43, 0x44 };

    check_layout (&value, 160, expected);
  }

  /* A width that is an exact multiple of the 64-bit limb, so that there
     is no padding at all on ppc64 and the two endiannesses are pure
     mirror images of each other.  */
  {
    unsigned _BitInt(128) value
      = ((unsigned _BitInt(128)) 0x0102030405060708u << 64)
      | (unsigned _BitInt(128)) 0x1112131415161718u;
    static const unsigned char expected[16]
      = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
	  0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18 };

    check_layout (&value, 128, expected);
  }

  return 0;
}
