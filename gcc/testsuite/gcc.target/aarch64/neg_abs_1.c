/* { dg-do compile } */
/* { dg-options "-save-temps -O2" } */

int
f1 (int x)
{
  return x < 0 ? x : -x;
}

long long
f2 (long long x)
{
  return x < 0 ? x : -x;
}

/* { dg-final { scan-assembler-not "\tneg\tw\[0-9\]*.*" } } */
/* { dg-final { scan-assembler-not "\tneg\tx\[0-9\]*.*" } } */
