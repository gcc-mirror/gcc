/* Verify the DWARF encoding of C99 floating point types.  */

/* { dg-do compile */
/* { dg-options "-O0 -gdwarf-2 -dA" } */
/* { dg-final { scan-assembler "0x4.*DW_AT_encoding" } } */
/* { dg-final { scan-assembler "0x4.*DW_AT_byte_size" } } */
/* { dg-final { scan-assembler "0x8.*DW_AT_byte_size" } } */
/* { dg-final { scan-assembler "0x10.*DW_AT_byte_size" } } */

void foo ()
{
  float f = 1.5f;
  double d = 1.5;
  long double l = 1.5l;
}
