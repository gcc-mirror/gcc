/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "" { *-*-aix* } } */
/* { dg-options "-O2 -dp -mcpu=power9 -mtune=power10" } */

/* Verify that the test case from PR108018 won't fail.  */

static int foo, bar;

unsigned long test(void)
{
        return (unsigned long)&foo + (unsigned long)&bar;
}

/* { dg-final { scan-assembler {\m.*largetocp10.*\M} } } */
/* { dg-final { scan-assembler {\m.*largetocp10_plus.*\M} } } */
