/* Check that we use the scalar variants of vector compares.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -fno-asynchronous-unwind-tables" } */

int
eq (double a, double b)
{
  return a == b;
}

/* { dg-final { scan-assembler "eq:\n\twfcedbs\t%v\[0-9\]*,%v0,%v2\n\tlhi\t%r1,0\n\tlhi\t%r2,1\n\tlocrne\t%r2,%r1" } } */

int
ne (double a, double b)
{
  return a != b;
}

/* { dg-final { scan-assembler "ne:\n\twfcedbs\t%v\[0-9\]*,%v0,%v2\n\tlhi\t%r1,0\n\tlhi\t%r2,1\n\tlocre\t%r2,%r1" } } */

int
gt (double a, double b)
{
  return a > b;
}

/* { dg-final { scan-assembler "gt:\n\twfchdbs\t%v\[0-9\]*,%v0,%v2\n\tlhi\t%r1,0\n\tlhi\t%r2,1\n\tlocrne\t%r2,%r1" } } */

int
ge (double a, double b)
{
  return a >= b;
}

/* { dg-final { scan-assembler "ge:\n\twfchedbs\t%v\[0-9\]*,%v0,%v2\n\tlhi\t%r1,0\n\tlhi\t%r2,1\n\tlocrne\t%r2,%r1" } } */

int
lt (double a, double b)
{
  return a < b;
}

/* { dg-final { scan-assembler "lt:\n\twfchdbs\t%v\[0-9\]*,%v2,%v0\n\tlhi\t%r1,0\n\tlhi\t%r2,1\n\tlocrne\t%r2,%r1" } } */

int
le (double a, double b)
{
  return a <= b;
}

/* { dg-final { scan-assembler "le:\n\twfchedbs\t%v\[0-9\]*,%v2,%v0\n\tlhi\t%r1,0\n\tlhi\t%r2,1\n\tlocrne\t%r2,%r1" } } */
