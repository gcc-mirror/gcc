/* PR target/67745
   Verify that -mthumb code is not aligned.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mthumb -fno-align-functions" }  */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */

void
foo()
{
}

/* { dg-final { scan-assembler-not ".align\[ \t]2" } } */
