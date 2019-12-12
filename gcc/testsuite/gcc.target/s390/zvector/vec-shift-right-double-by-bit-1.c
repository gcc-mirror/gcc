/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -fno-asynchronous-unwind-tables -dp" } */

#include <vecintrin.h>

vector unsigned char
foo1 (vector unsigned char a, vector unsigned char b)
{
  return vec_srdb (a, b, 0);
}

vector signed char
foo2 (vector signed char a, vector signed char b)
{
  return vec_srdb (a, b, 1);
}


vector unsigned short
foo3 (vector unsigned short a, vector unsigned short b)
{
  return vec_srdb (a, b, 2);
}

vector signed short
foo4 (vector signed short a, vector signed short b)
{
  return vec_srdb (a, b, 3);
}

vector unsigned int
foo5 (vector unsigned int a, vector unsigned int *b)
{
  return vec_srdb (a, *b, 4);
}

vector signed int
foo6 (vector signed int a, vector signed int b)
{
  return vec_srdb (a, b, 5);
}


vector unsigned long long
foo7 (vector unsigned long long a, vector unsigned long long b)
{
  return vec_srdb (a, (vector unsigned long long){ 1, 2 }, 6);
}

vector signed long long
foo8 (vector signed long long a, vector signed long long b)
{
  return vec_srdb (a, b, 7);
}


vector float
foo9 (vector float a, vector float b)
{
  return vec_srdb (a, b, 1);
}

vector double
foo10 (vector double a, vector double b)
{
  return vec_srdb (a, b, 3);
}

/* { dg-final { scan-assembler-times "vsrd" 10 } } */
