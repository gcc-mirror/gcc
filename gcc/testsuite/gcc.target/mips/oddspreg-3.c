/* Check that we disable odd-numbered single precision registers.  */
/* { dg-options "-mabi=32 -mfp32 -march=loongson3a -mhard-float -ffat-lto-objects" } */
/* This is testing for errors which can only happen in assembly generation.
   dg-error does not guarantee assembly generation, so we need to do it
   manually by using -ffat-lto-objects.  */

NOMIPS16 void
foo ()
{
  register float foo asm ("$f1"); /* { dg-error "isn't suitable for" } */
  asm volatile ("" : "=f" (foo));
}
