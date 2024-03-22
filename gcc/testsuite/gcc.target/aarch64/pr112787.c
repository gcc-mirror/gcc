/* { dg-do compile } */
/* { dg-additional-options "-O2 -march=armv8-a+sve -mcpu=neoverse-n2" } */

typedef int __attribute__((__vector_size__ (64))) vec;

vec fn (vec a, vec b)
{
  return a + b;
}

/* { dg-final { scan-assembler-times {add\tv[0-9]+} 4 } } */
