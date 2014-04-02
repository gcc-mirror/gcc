/* { dg-do compile } */
/* { dg-options "-O3 -m31 -mzarch" } */

void
foo ()
{
  asm ("" ::: "%f4");
}

/* { dg-final { scan-assembler "ld" } } */
