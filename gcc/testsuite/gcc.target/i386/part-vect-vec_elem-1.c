/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O1 -msse4.1" } */
/* { dg-require-effective-target sse4 } */

#include "sse4_1-check.h"

typedef _Float16 v4hf __attribute__((vector_size(8)));

v4hf
__attribute__((noipa))
vector_init_dupv4hf (_Float16 a)
{
  return __extension__(v4hf){a, a, a, a};
}

v4hf
__attribute__((noipa))
vector_init_allzero (_Float16 a)
{
  return __extension__(v4hf){0, 0, 0, 0};
}

v4hf
__attribute__((noipa))
vector_init_one_nonzero (_Float16 a)
{
  return __extension__(v4hf){0, 0, a, 0};
}

v4hf
__attribute__((noipa))
vector_init_one_var (_Float16 a)
{
  return __extension__(v4hf){1, 2, a, 4};
}

v4hf
__attribute__((noipa))
vector_init_general (_Float16 a, _Float16 a1, _Float16 a2, _Float16 a3)
{
  return __extension__(v4hf){a3, a2, a1, a};
}

v4hf
__attribute__((noipa))
vec_set  (_Float16 a, v4hf b)
{
  b[1] = a;
  return b;
}

v4hf
__attribute__((noipa))
vec_set_var  (_Float16 a, v4hf b, int c)
{
  b[c] = a;
  return b;
}

_Float16
__attribute__((noipa))
vec_extract  (v4hf b)
{
  return b[2];
}

static void
sse4_1_test ()
{
  typedef union {
    _Float16 a[4];
    v4hf x;}union64hf;
  union64hf res, exp, src;

  res.x = vector_init_dupv4hf (1.0f16);
  for (int i = 0; i != 4; i++)
    exp.a[i] = 1.0f16;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 8) != 0)
    __builtin_abort ();

  res.x = vector_init_allzero (1.0f16);
  for (int i = 0; i != 4; i++)
    exp.a[i] = 0.0f16;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 8) != 0)
    __builtin_abort ();

  res.x = vector_init_one_nonzero (1.0f16);
  for (int i = 0; i != 4; i++)
    exp.a[i] = 0.0f16;
  exp.a[2] = 1.0f16;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 8) != 0)
    __builtin_abort ();

  res.x = vector_init_one_var (3.0f16);
  for (int i = 0; i != 4; i++)
    exp.a[i] = i + 1;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 8) != 0)
    __builtin_abort ();

  res.x = vector_init_general (4.0, 3.0f, 2.0f, 1.0);
  for (int i = 0; i != 4; i++)
    exp.a[i] = 1 + i;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 8) != 0)
    __builtin_abort ();

  for (int i = 0; i != 4; i++)
    {
      src.a[i] = i;
      exp.a[i] = i;
    }
  res.x = vec_set (3.0f, src.x);
  exp.a[1] = 3.0f;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 8) != 0)
    __builtin_abort ();

  for (int i = 0; i != 4; i++)
    {
      src.a[i] = i;
      exp.a[i] = i;
    }
  res.x = vec_set_var (3.0f, src.x, 1);
  exp.a[1] = 3.0f;
  if (__builtin_memcmp (&res.a[0], &exp.a[0], 8) != 0)
    __builtin_abort ();

  for (int i = 0; i != 4; i++)
    {
      src.a[i] = i;
      exp.a[i] = i;
    }
  _Float16 res_scalar = vec_extract (src.x);
  if (res_scalar != 2.0f)
    __builtin_abort ();
  return ;
}
