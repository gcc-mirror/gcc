/* { dg-do compile { target { arm_arm_ok || arm_thumb2_ok} } } */
/* { dg-options "-O1" } */

unsigned long long or64 (unsigned long long input)
{
    return input | 0x200000004ULL;
}

/* { dg-final { scan-assembler-not "mov\[\\t \]+.+,\[\\t \]*.+" } } */
