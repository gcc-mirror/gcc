/* { dg-do compile { target lto } }
/* { dg-options "-O2 -flto" } */

void foo ()
{
}

/* Check that scan-assembler* directives skip the LTO section.  */
/* { dg-final { scan-assembler-not "ascii" } } */
/* { dg-final { scan-assembler-times "ascii" 0 } } */
