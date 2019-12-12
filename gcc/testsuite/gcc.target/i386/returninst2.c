/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-pg -mfentry -minstrument-return=nop5 -mrecord-return" } */
/* { dg-final { scan-assembler-times "0x0f, 0x1f, 0x44, 0x00, 0x00" 3 } } */
/* { dg-final { scan-assembler "section.*return_loc" } } */

int func(int a)
{
  return a+1;
}

int func2(int a)
{
  return a+1;
}

extern void func4(int);

int func3(int a)
{
  func4(a + 1);
}
