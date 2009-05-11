/* { dg-do compile } */
/* { dg-options "-O2 -Wunreachable-code" } */
float Factorial(float X)
{
  float val = 1.0;
  int k,j;
  for (k=1; k < 5; k++) /* { dg-bogus "will never be executed" "" { xfail *-*-* } } */
    {
      val += 1.0; /* { dg-bogus "will never be executed" "" { xfail *-*-* } } */
    }
  return (val); /* { dg-bogus "will never be executed" } */
}

int main (void)
{
  float result;
  result=Factorial(2.1);
  return (0);
}

