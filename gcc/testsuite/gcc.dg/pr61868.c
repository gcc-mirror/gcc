/* { dg-do compile } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -frandom-seed=0x12345" }  */
extern int foo (int);
int main ()
{
  foo (100);
  return 0;
}
/* { dg-final { scan-raw-assembler "\.gnu\.lto.*.12345" } } */
