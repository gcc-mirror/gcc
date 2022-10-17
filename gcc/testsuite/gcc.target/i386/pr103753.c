/* { dg-do compile } */
/* { dg-options "-mavx2 -mno-avx512f -O2" } */
/* { dg-final { scan-assembler-not "vpbroadcastw" } } */

typedef _Float16 __v16hf __attribute__ ((__vector_size__ (32)));

__v16hf foo (_Float16 x)
{
  return (__v16hf) { x, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f };
}

typedef short __v16hi __attribute__ ((__vector_size__ (32)));

__v16hi bar (short x)
{
  return (__v16hi) { x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
}
