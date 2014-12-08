/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f" } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  0 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vbroadcastss\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 0 } } */
/* { dg-final { scan-assembler-times "vbroadcastsd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 0 } } */

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
