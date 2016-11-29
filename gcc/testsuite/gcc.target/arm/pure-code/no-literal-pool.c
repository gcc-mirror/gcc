/* { dg-do compile } */
/* { dg-options "-mpure-code" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-fpic" "-fPIC" } { "" } } */

float sf;
double df;
long long l;
static char *p = "Hello World";

float
testsf (float *p)
{
  if (*p > 1.1234f)
    return 2.1234f;
  else
    return 3.1234f;
}

double
testdf (double *p)
{
  if (*p > 4.1234)
    return 2.1234;
  else
    return 3.1234;
}

long long
testll (long long *p)
{
  if (*p > 0x123456789ABCDEFll)
    return 0x111111111ll;
  else
    return 0x222222222ll;
}

char *
testchar ()
{
  return p + 4;
}

int
foo (int a, int b)
{
  int i;
  volatile int *labelref = &&label1;

  if (a > b)
    {
      while (i < b)
	{
	  a += *labelref;
	  i += 1;
	}
      goto *labelref;
    }
  else
    b = b + 3;

  a = a * b;

label1:
  return a + b;
}

/* { dg-final { scan-assembler-not "\\.(float|l\\?double|\d?byte|short|int|long|quad|word)\\s+\[^.\]" } } */
/* { dg-final { scan-assembler "text,\"0x20000006\"" } } */
