/* Verify that CET works.  */
/* { dg-do compile } */
/* { dg-options "-O -fcf-protection" } */
/* { dg-final { scan-assembler-times "endbr32" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 3 { target { ! ia32 } } } } */

__attribute__ ((noinline, noclone))
static int
func (int arg)
{
  static void *array[] = { &&foo, &&bar };

  goto *array[arg];
foo:
  return arg*111;
bar:
  return arg*777;
}

int
foo (int arg)
{
  return func (arg);
}
