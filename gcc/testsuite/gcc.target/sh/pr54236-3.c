/* Tests to check the utilization of the addc and subc instructions.
   If everything works as expected we won't see any movt instructions in
   these cases.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } } */
/* { dg-final { scan-assembler-times "addc" 1 } } */
/* { dg-final { scan-assembler-times "subc" 1 } } */
/* { dg-final { scan-assembler-not "movt" } } */

int
test_000 (int* x, unsigned int c)
{
  /* 1x addc  */
  int s = 0;
  unsigned int i;
  for (i = 0; i < c; ++i)
    s += ! (x[i] & 0x3000);
  return s;
}

int
test_001 (int* x, unsigned int c)
{
  /* 1x subc  */
  int s = 0;
  unsigned int i;
  for (i = 0; i < c; ++i)
    s -= ! (x[i] & 0x3000);
  return s;
}
