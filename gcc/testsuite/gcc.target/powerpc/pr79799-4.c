/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <altivec.h>
#include <stdlib.h>

__attribute__ ((__noinline__))
vector float
insert_0 (vector float v, float f)
{
  return vec_insert (f, v, 0);
}

__attribute__ ((__noinline__))
vector float
insert_1 (vector float v, float f)
{
  return vec_insert (f, v, 1);
}

__attribute__ ((__noinline__))
vector float
insert_2 (vector float v, float f)
{
  return vec_insert (f, v, 2);
}

__attribute__ ((__noinline__))
vector float
insert_3 (vector float v, float f)
{
  return vec_insert (f, v, 3);
}

__attribute__ ((__noinline__))
void
test_insert (void)
{
  vector float v1 = { 1.0f, 2.0f, 3.0f, 4.0f };
  vector float v2 = { 5.0f, 6.0f, 7.0f, 8.0f };

  v1 = insert_0 (v1, 5.0f);
  v1 = insert_1 (v1, 6.0f);
  v1 = insert_2 (v1, 7.0f);
  v1 = insert_3 (v1, 8.0f);

  if (vec_any_ne (v1, v2))
    abort ();
}

__attribute__ ((__noinline__))
vector float
insert_extract_0_3 (vector float v1, vector float v2)
{
  return vec_insert (vec_extract (v2, 3), v1, 0);
}

__attribute__ ((__noinline__))
vector float
insert_extract_1_2 (vector float v1, vector float v2)
{
  return vec_insert (vec_extract (v2, 2), v1, 1);
}

__attribute__ ((__noinline__))
vector float
insert_extract_2_1 (vector float v1, vector float v2)
{
  return vec_insert (vec_extract (v2, 1), v1, 2);
}

__attribute__ ((__noinline__))
vector float
insert_extract_3_0 (vector float v1, vector float v2)
{
  return vec_insert (vec_extract (v2, 0), v1, 3);
}

__attribute__ ((__noinline__))
void
test_insert_extract (void)
{
  vector float v1 = { 1.0f, 2.0f, 3.0f, 4.0f };
  vector float v2 = { 5.0f, 6.0f, 7.0f, 8.0f };
  vector float v3 = { 8.0f, 7.0f, 6.0f, 5.0f };

  v1 = insert_extract_0_3 (v1, v2);
  v1 = insert_extract_1_2 (v1, v2);
  v1 = insert_extract_2_1 (v1, v2);
  v1 = insert_extract_3_0 (v1, v2);

  if (vec_any_ne (v1, v3))
    abort ();
}

int
main (void)
{
  test_insert ();
  test_insert_extract ();
  return 0;
}
