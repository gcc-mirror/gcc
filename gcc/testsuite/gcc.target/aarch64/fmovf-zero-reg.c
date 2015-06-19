/* { dg-do compile } */
/* { dg-options "-O2" } */

void bar (float);
void
foo (void)
{
  bar (0.0);
}

/* { dg-final { scan-assembler "fmov\\ts0, wzr" } } */
