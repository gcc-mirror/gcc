/* Check that we enable odd-numbered single precision registers.  */
/* { dg-options "-mabi=32 -modd-spreg -mhard-float" } */

#if _MIPS_SPFPSET != 32
#error "Incorrect number of single-precision registers reported"
#endif

NOMIPS16 void
foo ()
{
  register float foo asm ("$f1");
  asm volatile ("" : "=f" (foo));
}
