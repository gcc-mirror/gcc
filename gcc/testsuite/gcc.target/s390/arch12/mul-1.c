/* { dg-do compile } */

int
msrkc (int unused, int a, int b)
{
  return a * b;
}

long long
msgrkc (int unused, long long a, long long b)
{
  return a * b;
}

/* Make sure the 2 operand version are still being used.  */

int
msr (int a, int b)
{
  return a * b;
}

long long
msgr (long long a, long long b)
{
  return a * b;
}

/* { dg-final { scan-assembler-times "\tmsrkc\t" 1 } } */
/* { dg-final { scan-assembler-times "\tmsgrkc\t" 1 } } */
