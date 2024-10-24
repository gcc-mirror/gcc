/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

int
f1 (int x)
{
  x /= 2;
  return x * svcntw();
}

int
f2 (int x)
{
  x /= 4;
  return x * svcnth();
}

int
f3 (int x)
{
  x /= 8;
  return x * svcntb();
}

int
f4 (int x)
{
  x /= 2;
  return x * svcnth();
}

int
f5 (int x)
{
  x /= 4;
  return x * svcntb();
}

int
f6 (int x)
{
  x /= 2;
  return x * svcntb();
}

int
f7 (int x)
{
  x /= 16;
  return x * svcntb() * 16;
}

/* { dg-final { scan-assembler-times {\tasr\t} 7 } } */
