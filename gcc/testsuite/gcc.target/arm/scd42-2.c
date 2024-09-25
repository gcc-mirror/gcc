/* Verify that mov is preferred on XScale for loading a 2 byte constant. */
/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_xscale_arm_ok } */
/* { dg-options "-O" } */
/* { dg-add-options arm_cpu_xscale_arm } */

unsigned load2(void) __attribute__ ((naked));
unsigned load2(void)
{
    /* Best code would be:
       mov r0, =272
       add r0, r0, =1
       mov pc, lr */

    return 273;
}

/* { dg-final { scan-assembler "mov\[ 	].*272" } } */
