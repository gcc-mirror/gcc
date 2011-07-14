/* { dg-do compile } */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-options "-Os -mthumb" } */

int returnbool(int a, int b)
{
    if (a < b)
        return 1;
    return 0;
}

/* { dg-final { scan-assembler-not "eor" } } */
