/* PR target/89355  */
/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-final { scan-assembler-times "endbr32" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 1 { target { ! ia32 } } } } */
int
test (int* val)
{
  int status = 99;

  if (!val)
    {
      status = 22;
      goto end;
    }

  extern int x;
  *val = x;

  status = 0;
end:
  return status;
}
