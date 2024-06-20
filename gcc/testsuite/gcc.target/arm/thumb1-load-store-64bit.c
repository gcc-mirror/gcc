/* { dg-do compile } */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb1_ok } */

void copy_df(double *dst, const double *src)
{
    *dst = *src;
}

void copy_di(unsigned long long *dst, const unsigned long long *src)
{
    *dst = *src;
}

/* { dg-final { scan-assembler-times "ldmia\tr\[0-7\]" 2 } } */
/* { dg-final { scan-assembler-times "stmia\tr\[0-7\]!" 2 } } */
