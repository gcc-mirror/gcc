/* Verify that hardware multiply is preferred on XScale. */
/* { dg-do compile { target xscale*-*-* } } */
/* { dg-options "-mcpu=xscale -O2" } */

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

/* We want to suppress running for -mthumb but not for -mthumb-interwork. */
/* { dg-final { global compiler_flags; if ![string match "*-mthumb *" $compiler_flags] { scan-assembler "mla\[ 	].*" } } } */
