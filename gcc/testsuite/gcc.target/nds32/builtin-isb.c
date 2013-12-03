/* Verify that we generate isb instruction with builtin function.  */

/* { dg-do compile }  */
/* { dg-options "-O0" }  */
/* { dg-final { scan-assembler "\\tisb" } }  */

void
test (void)
{
  __builtin_nds32_isb ();
}
