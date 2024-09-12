/* Verify that hardware multiply is preferred on XScale. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_cpu_xscale_arm_ok } */
/* { dg-add-options arm_cpu_xscale_arm } */


/* Brett Gaines' test case. */
unsigned BCPL(unsigned) __attribute__ ((naked));
unsigned BCPL(unsigned seed)
{
    /* Best code would be:
       ldr r1, =2147001325
       ldr r2, =715136305
       mla r0, r1, r0, r2
       mov pc, lr */

    return seed * 2147001325U + 715136305U;
}

/* { dg-final { scan-assembler "mla\[ 	].*" } } */
