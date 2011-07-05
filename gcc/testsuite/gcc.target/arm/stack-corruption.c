/* { dg-do compile } */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-options "-O -mthumb -fno-omit-frame-pointer" } */

int main() {
  return 0;
}

/* { dg-final { scan-assembler-not "\tadd\tr7, sp, #8\n" } } */
