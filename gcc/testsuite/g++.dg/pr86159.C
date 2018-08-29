// { dg-do compile }
// { dg-options "-O -w -Wno-psabi" }

typedef int vec __attribute__((vector_size(32)));
vec fn1()
{
  vec x, zero{};
  vec one = zero + 1;
  return x < zero ? one : zero;
}
