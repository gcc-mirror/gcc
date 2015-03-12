/* { dg-do compile { target { ! lp64 } } } */
/* { dg-options "-O3 -mzarch" } */

void
foo ()
{
  asm ("" ::: "%f4");
}

/* { dg-final { scan-assembler "ld" } } */
