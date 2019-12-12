/* PR target/89355  */
/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection -Wno-return-local-addr" } */
/* { dg-final { scan-assembler-times "endbr32" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 1 { target { ! ia32 } } } } */

void *
func (void)
{
  return &&bar;
bar:
  return 0;
}
