/* Check for thumb1 imm [255-510] moves.  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-options "-Os" } */
/* { dg-skip-if "" { ! { arm_thumb1 } } } */

int f()
{
  return 257;
}

/* { dg-final { scan-assembler-not "ldr" } } */

