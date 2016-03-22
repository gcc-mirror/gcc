/* { dg-do compile } */
/* { dg-additional-options "-mavx512vl" { target { i?86-*-* x86_64-*-* } } } */

typedef int vec __attribute__((vector_size(32)));

vec
test1 (vec x,vec y)
{
  return (x < y) ? 1 : 0;
}

vec
test2 (vec x,vec y)
{
  vec zero = { };
  vec one = zero + 1;
  return (x < y) ? one : zero;
}
