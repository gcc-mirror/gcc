/* Verify that mov is preferred on XScale for loading a 2 byte constant. */
/* { dg-do compile { target xscale-*-* } } */
/* { dg-options "-mcpu=xscale -O" } */

unsigned load2(void) __attribute__ ((naked));
unsigned load2(void)
{
    /* Best code would be:
       mov r0, =272
       add r0, r0, =1
       mov pc, lr */

    return 273;
}

/* We want to suppress running for -mthumb but not for -mthumb-interwork. */
/* { dg-final { global compiler_flags; if ![string match "*-mthumb *" $compiler_flags] { scan-assembler "mov\[ 	].*272" } } } */
