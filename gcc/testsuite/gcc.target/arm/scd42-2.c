/* Verify that mov is preferred on XScale for loading a 2 byte constant. */
/* { dg-do compile } */
/* { dg-skip-if "Test is specific to the Xscale" { arm*-*-* } { "-march=*" } { "-march=xscale" } } */
/* { dg-skip-if "Test is specific to the Xscale" { arm*-*-* } { "-mcpu=*" } { "-mcpu=xscale" } } */
/* { dg-skip-if "Test is specific to ARM mode" { arm*-*-* } { "-mthumb" } { "" } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-mcpu=xscale -O -marm" } */

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
