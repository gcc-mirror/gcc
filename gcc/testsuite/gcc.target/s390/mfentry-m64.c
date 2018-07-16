/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-pg -mfentry" } */

void
profileme (void)
{
  /* { dg-final { scan-assembler "brasl\t0,__fentry__" } } */
}
