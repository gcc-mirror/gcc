/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { } { "powerpc*-*-aix*" } } */
/* { dg-options "-O2 -dp -mcpu=power9 -mtune=power10" } */
/* { dg-additional-options "-mcmodel=large" } */

/* Verify that the test case from PR108018 won't fail.  */

static int foo, bar;

unsigned long test(void)
{
        return (unsigned long)&foo + (unsigned long)&bar;
}

/* { dg-final { scan-assembler-times {\m.*largetocp10_aix.*\M} 2 } } */
