/* { dg-do compile { target { tune_cortex_a76 } } } */
/* { dg-additional-options " -dA -march=armv8.6-a " } */

void foo ()
{}

/* { dg-final { scan-assembler "//.tune cortex-a76" } } */
/* { dg-final { scan-assembler ".arch armv8.6-a" } } */
