// { dg-additional-options "-fmodules-ts -fopenmp-simd" }
// { dg-require-effective-target pthread }

import foo;

unsigned ary[64];

int main ()
{
  frob (ary);
}
