/* Check that move instructions have the correct length on SH2A.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -dp" }  */

/* { dg-final { scan-assembler-times "length = 4" 10 { target { "sh2a" && any_fpu } } } }  */
/* { dg-final { scan-assembler-times "length = 4" 8 { target { "sh2a" && no_fpu } } } }  */

int
test_00 (int* x)
{
  return x[0];
}

int
test_01 (int* x)
{
  return x[1];
}

int
test_02 (int* x)
{
  return x[100];
}

int
test_03 (int* x, unsigned int y)
{
  return *(int*)((unsigned int)x + y);
}

float
test_04 (float* x)
{
  return x[0];
}

float
test_05 (float* x)
{
  return x[5];
}

float
test_06 (float* x)
{
  return x[100];
}

int
test_07 (void)
{
  return 1230;
}

int
test_08 (void)
{
  return 0xFF0000;
}
