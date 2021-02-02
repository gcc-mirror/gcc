/* { dg-do compile { target { tune_cortex_a76 } } } */
/* { dg-additional-options " -dA " } */

void foo ()
{}

/* { dg-final { scan-assembler "//.tune cortex-a76" } } */
