/* Ensure that explicit zero-entend instructions are present when compiling
   for targets that may not have sram-ecc enabled (in which sub-dword loads do
   not zero the high bits of the target register).  */

/* { dg-do compile } */
/* { dg-options "-O2 -msram-ecc=any" } */

extern unsigned char c;

unsigned int
f ()
{
  return c;
}

/* { dg-final { scan-assembler "lshl.* 24" } } */
/* { dg-final { scan-assembler "lshr.* 24" } } */
