/* Verify that we generate setgie.e instruction with builtin function.  */

/* { dg-do compile }  */
/* { dg-options "-O0" }  */
/* { dg-final { scan-assembler "\\tsetgie.e" } }  */

void
test (void)
{
  __builtin_nds32_setgie_en ();
}
