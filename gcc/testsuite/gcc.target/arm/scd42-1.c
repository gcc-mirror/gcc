/* Verify that mov is preferred on XScale for loading a 1 byte constant. */
/* { dg-do compile } */
/* { dg-options "-mcpu=xscale -O" } */

unsigned load1(void) __attribute__ ((naked));
unsigned load1(void)
{
    /* Best code would be:
       mov r0, =17
       mov pc, lr */

    return 17;
}

/* { dg-final { scan-assembler "mov\[ 	].*17" } } */
