/* { dg-options "-O2" } */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */

unsigned long long a;
unsigned int b;
unsigned short c;

unsigned long long mul32()
{
  return a * b;
}

unsigned long long mul16()
{
  return a * c;
}

/* { dg-final { scan-assembler-not "xor" } } */
