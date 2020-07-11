/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -O2" } */

typedef unsigned uint32_t __attribute__((mode (__SI__)));

float foo ()
{
  uint32_t MASK = 0x80000000;
  return (float &) MASK; /* { dg-warning "strict-aliasing" } */
}

