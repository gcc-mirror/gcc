/* { dg-do compile { target { tune_cortex_a76 } } } */
/* { dg-additional-options " -dA -mtune=cortex-a73" } */

void foo ()
{}

/* { dg-final { scan-assembler "//.tune cortex-a73" } } */
