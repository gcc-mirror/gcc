/* { dg-do compile } */
/* { dg-options "-O2" } */

void bar (long double);
void
foo (void)
{
  bar (0.0);
}

/* { dg-final { scan-assembler "movi\\tv0\.2d, #0" } } */
