/* Verify that ldr is preferred on XScale for loading a 3 or 4 byte constant. */
/* { dg-do compile { target xscale-*-* } } */
/* { dg-options "-mcpu=xscale -O" } */

unsigned load4(void) __attribute__ ((naked));
unsigned load4(void)
{
    /* Best code would be:
       ldr r0, =65809
       mov pc, lr */

    return 65809;
}

/* { dg-final { scan-assembler "ldr\[ 	].*" } } */
