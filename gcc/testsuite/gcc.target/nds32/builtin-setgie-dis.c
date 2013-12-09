/* Verify that we generate setgie.d instruction with builtin function.  */

/* { dg-do compile }  */
/* { dg-options "-O0" }  */
/* { dg-final { scan-assembler "\\tsetgie.d" } }  */

void
test (void)
{
  __builtin_nds32_setgie_dis ();
}
