/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f" } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+%zmm" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastb" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw" 2 } } */
/* { dg-final { scan-assembler-times "vbroadcastss" 1 } } */
/* { dg-final { scan-assembler-times "vbroadcastsd" 1 } } */

#include <x86intrin.h>

typedef char __v64qi __attribute__ ((vector_size (64)));
typedef short __v32hi __attribute__ ((vector_size (64)));

__v64qi foo_1 (char c)
{
  __v64qi v1 = {
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c
  };

  return v1;
}

__v32hi foo_2 (short c)
{
  __v32hi v1 = {
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c
  };

  return v1;
}

__v16si foo_3 (int c)
{
  __v16si v1 = {
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c
  };

  return v1;
}

__v8di foo_4 (long long c)
{
  __v8di v1 = {
      c, c, c, c, c, c, c, c
  };

  return v1;
}

__v32qi foo_5 (char c)
{
  __v32qi v1 = {
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c
  };

  return v1;
}

__v16hi foo_6 (short c)
{
  __v16hi v1 = {
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c
  };

  return v1;
}

__v8si foo_7 (int c)
{
  __v8si v1 = {
      c, c, c, c, c, c, c, c
  };

  return v1;
}

__v4di foo_8 (long long c)
{
  __v4di v1 = {
      c, c, c, c
  };

  return v1;
}


__v16qi foo_9 (char c)
{
  __v16qi v1 = {
      c, c, c, c, c, c, c, c,
      c, c, c, c, c, c, c, c
  };

  return v1;
}

__v8hi foo_10(short c)
{
  __v8hi v1 = {
      c, c, c, c, c, c, c, c
  };

  return v1;
}
