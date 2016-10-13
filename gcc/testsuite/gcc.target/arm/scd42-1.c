/* Verify that mov is preferred on XScale for loading a 1 byte constant. */
/* { dg-do compile } */
/* { dg-skip-if "Test is specific to Xscale" { arm*-*-* } { "-march=*" } { "-march=xscale" } } */
/* { dg-skip-if "Test is specific to Xscale" { arm*-*-* } { "-mcpu=*" } { "-mcpu=xscale" } } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=softfp" } } */
/* { dg-options "-mcpu=xscale -O -mfloat-abi=softfp" } */

unsigned load1(void) __attribute__ ((naked));
unsigned load1(void)
{
    /* Best code would be:
       mov r0, =17
       mov pc, lr */

    return 17;
}

/* { dg-final { scan-assembler "mov\[ 	].*17" { target { arm_nothumb } } } } */
/* { dg-final { scan-assembler "movs\[ 	].*17" { target { ! arm_nothumb } } } } */
