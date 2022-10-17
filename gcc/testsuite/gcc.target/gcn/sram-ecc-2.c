/* Ensure that explicit zero-entend instructions are present when compiling
   for targets without sram-ecc enabled (in which sub-dword loads do not
   zero the high bits of the target register).  */

/* { dg-do compile } */
/* { dg-options "-O2 -msram-ecc=off" } */

extern unsigned short s;

unsigned short
f ()
{
  return s;
}

/* { dg-final { scan-assembler "lshl.* 16" } } */
/* { dg-final { scan-assembler "lshr.* 16" } } */
