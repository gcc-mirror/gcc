// PR target/82112
// { dg-do compile }
// { dg-options "-maltivec" }
// { dg-require-effective-target powerpc_altivec }

#include <altivec.h>

__attribute__((aligned (16))) extern const unsigned char c[16];

void
foo (void)
{
  vec_ld (0, c);
}
