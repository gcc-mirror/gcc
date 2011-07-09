/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -fzee" } */
/* { dg-final { scan-assembler-not "mov\[\\t \]+\(%\[\^,\]+\),\[\\t \]*\\1" } } */
int mask[100];
int foo(unsigned x)
{
  if (x < 10)
    x = x * 45;
  else
    x = x * 78;
  return mask[x];
}
