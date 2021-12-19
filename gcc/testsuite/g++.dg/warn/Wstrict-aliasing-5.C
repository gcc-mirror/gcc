/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -O2" } */

typedef unsigned uint32_t __attribute__((mode (__SI__)));

float foo ()
{
  uint32_t MASK = 0x80000000;
  float f1 = (float &)MASK; // OK, same as const_cast<float &>(static_cast<float const &>(MASK))
  return reinterpret_cast<float &>(MASK); /* { dg-warning "strict-aliasing" } */
}

