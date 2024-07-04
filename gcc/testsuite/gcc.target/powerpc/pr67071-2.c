/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-mdejagnu-cpu=power6 -maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

vector unsigned char
foo_char (void)
{
  return (vector unsigned char) {
#if __VEC_ELEMENT_REG_ORDER__ == __ORDER_BIG_ENDIAN__
    0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
#else
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80
#endif
  };
}

vector unsigned short
foo_short (void)
{
  return (vector unsigned short) {
#if __VEC_ELEMENT_REG_ORDER__ == __ORDER_BIG_ENDIAN__
    0x8000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000
#else
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x8000
#endif
  };
}

vector unsigned int
foo_int (void)
{
  return (vector unsigned int) {
#if __VEC_ELEMENT_REG_ORDER__ == __ORDER_BIG_ENDIAN__
    0x80000000u, 0x00000000u, 0x00000000u, 0x00000000u,
#else
    0x00000000u, 0x00000000u, 0x00000000u, 0x80000000u,
#endif
  };
}

/* { dg-final { scan-assembler-times "vspltisw" 3 } } */
/* { dg-final { scan-assembler-times "vsldoi"   3 } } */
/* { dg-final { scan-assembler-times "vslb"     1 } } */
/* { dg-final { scan-assembler-times "vslh"     1 } } */
/* { dg-final { scan-assembler-times "vslw"     1 } } */
