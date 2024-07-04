/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-mdejagnu-cpu=power6 -maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

vector unsigned char
foo_char (void)
{
  return (vector unsigned char) {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80
  };
}

vector unsigned short
foo_short (void)
{
  return (vector unsigned short) {
    0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000
  };
}

vector unsigned int
foo_int (void)
{
  return (vector unsigned int) {
    0x80000000u, 0x80000000u, 0x80000000u, 0x80000000u,
  };
}

/* { dg-final { scan-assembler-times "vspltisw" 3 } } */
/* { dg-final { scan-assembler-times "vslb"     1 } } */
/* { dg-final { scan-assembler-times "vslh"     1 } } */
/* { dg-final { scan-assembler-times "vslw"     1 } } */
