/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-options "-mthumb -Os" }  */
/* { dg-final { scan-assembler-not "ldr" } } */

unsigned int foo (unsigned int i )
{
  return i | 0xff000000;
}
