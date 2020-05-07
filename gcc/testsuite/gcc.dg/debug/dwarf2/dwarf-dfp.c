/* Verify the DWARF encoding of C99 decimal floating point types.  */

/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O0 -gdwarf -dA" } */
/* { dg-final { scan-assembler "0x10.*DW_AT_encoding" } } */
/* { dg-final { scan-assembler "0x4.*DW_AT_byte_size" } } */
/* { dg-final { scan-assembler "0x8.*DW_AT_byte_size" } } */
/* { dg-final { scan-assembler "0x10.*DW_AT_byte_size" } } */

void foo ()
{
  _Decimal32 f = 1.5df;
  _Decimal64 d = 1.5dd;
  _Decimal128 l = 1.5dl;
}
