/* Check that we use the scalar variants of vector compares.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler-times "wfcedbs\t%v\[0-9\]*,%v0,%v2" 2 } } */
/* { dg-final { scan-assembler-times "wfchdbs\t%v\[0-9\]*,%v0,%v2" 1 } } */
/* { dg-final { scan-assembler-times "wfchedbs\t%v\[0-9\]*,%v2,%v0" 1 } } */
/* { dg-final { scan-assembler-times "wfchdbs\t%v\[0-9\]*,%v2,%v0" 1 } } */
/* { dg-final { scan-assembler-times "wfchedbs\t%v\[0-9\]*,%v2,%v0" 1 } } */
/* { dg-final { scan-assembler-times "lochine" 5 } } */
/* { dg-final { scan-assembler-times "lochino" 1 } } */


int
eq (double a, double b)
{
  return a == b;
}

int
ne (double a, double b)
{
  return a != b;
}

int
gt (double a, double b)
{
  return a > b;
}

int
ge (double a, double b)
{
  return a >= b;
}

int
lt (double a, double b)
{
  return a < b;
}

int
le (double a, double b)
{
  return a <= b;
}
