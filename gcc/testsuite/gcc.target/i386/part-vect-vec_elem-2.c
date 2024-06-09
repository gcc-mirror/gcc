/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O1 -msse4.1" } */
/* { dg-require-effective-target sse4 } */

#include "sse4_1-check.h"

typedef _Float16 v2hf __attribute__((vector_size(4)));

v2hf
__attribute__((noipa))
vector_init_dupv2hf (_Float16 a)
{
  return __extension__(v2hf){a, a};
}

v2hf
__attribute__((noipa))
vector_init_allzero (_Float16 a)
{
  return __extension__(v2hf){0, 0};
}

v2hf
__attribute__((noipa))
vector_init_one_nonzero (_Float16 a)
{
  return __extension__(v2hf){0, a};
}

v2hf
__attribute__((noipa))
vector_init_one_var (_Float16 a)
{
  return __extension__(v2hf){1, a};
}

v2hf
__attribute__((noipa))
vector_init_general (_Float16 a1, _Float16 a2)
{
  return __extension__(v2hf){a2, a1};
}

v2hf
__attribute__((noipa))
vec_set  (_Float16 a, v2hf b)
{
  b[1] = a;
  return b;
}

v2hf
__attribute__((noipa))
vec_set_var  (_Float16 a, v2hf b, int c)
{
  b[c] = a;
  return b;
}

_Float16
__attribute__((noipa))
vec_extract  (v2hf b)
{
  return b[1];
}

static void
sse4_1_test ()
{
  typedef union {
    _Float16 a[2];
    v2hf x;}union64hf;
  union64hf res, exp, src;

  res.x = vector_init_dupv2hf (1.0f16);
  for (int i = 0; i != 2; i++)
    exp.a[i] = 1.0f16;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 4) != 0)
    __builtin_abort ();

  res.x = vector_init_allzero (1.0f16);
  for (int i = 0; i != 2; i++)
    exp.a[i] = 0.0f16;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 4) != 0)
    __builtin_abort ();

  res.x = vector_init_one_nonzero (1.0f16);
  for (int i = 0; i != 2; i++)
    exp.a[i] = 0.0f16;
  exp.a[1] = 1.0f16;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 4) != 0)
    __builtin_abort ();

  res.x = vector_init_one_var (3.0f16);
  exp.a[0] = 1;
  exp.a[1] = 3;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 4) != 0)
    __builtin_abort ();

  res.x = vector_init_general (2.0f, 1.0);
  for (int i = 0; i != 2; i++)
    exp.a[i] = 1 + i;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 4) != 0)
    __builtin_abort ();

  for (int i = 0; i != 2; i++)
    {
      src.a[i] = i;
      exp.a[i] = i;
    }
  res.x = vec_set (3.0f, src.x);
  exp.a[1] = 3.0f;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 4) != 0)
    __builtin_abort ();

  for (int i = 0; i != 2; i++)
    {
      src.a[i] = i;
      exp.a[i] = i;
    }
  res.x = vec_set_var (3.0f, src.x, 1);
  exp.a[1] = 3.0f;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 4) != 0)
    __builtin_abort ();

  for (int i = 0; i != 2; i++)
    {
      src.a[i] = i;
      exp.a[i] = i;
    }
  _Float16 res_scalar = vec_extract (src.x);
  if (res_scalar != 1.0f)
    __builtin_abort ();
  return ;
}
