/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-pg -mfentry -fPIC" } */

void
profileme (void)
{
  /* __fentry__ must be referenced through PLT.  */
  /* { dg-final { scan-assembler "brasl\t0,__fentry__@PLT\n" } } */
}
