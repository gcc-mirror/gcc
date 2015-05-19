/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

typedef __attribute__((vector_size(16))) signed int v4si;

v4si
adddbl (v4si a, v4si b)
{
  return a + b;
}

v4si
subdbl (v4si a, v4si b)
{
  return a - b;
}

v4si
muldbl (v4si a, v4si b)
{
  return a * b;
}

v4si
divdbl (v4si a, v4si b)
{
  return a / b;
}

v4si
fmadbl (v4si a, v4si b, v4si c)
{
  return a * b + c;
}

v4si
fmsdbl (v4si a, v4si b, v4si c)
{
  return a * b - c;
}
