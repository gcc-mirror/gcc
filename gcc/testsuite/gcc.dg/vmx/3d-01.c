/* { dg-do compile } */
#include <altivec.h>
int i;

void
test_vec_all_gt(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_all_gt(u8a, u8b))
    i = 1;
}

void
test_vec_all_le(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_all_le(u8a, u8b))
    i = 1;
}

void
test_vec_any_gt(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_any_gt(u8a, u8b))
    i = 1;
}

void
test_vec_any_le(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_any_le(u8a, u8b))
    i = 1;
}

void
test_vec_all_lt(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_all_lt(u8a, u8b))
    i = 1;
}

void
test_vec_all_ge(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_all_ge(u8a, u8b))
    i = 1;
}

void
test_vec_any_lt(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_any_lt(u8a, u8b))
    i = 1;
}

void
test_vec_any_ge(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_any_ge(u8a, u8b))
    i = 1;
}

void
test_vec_all_eq(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_all_eq(u8a, u8b))
    i = 1;
}

void
test_vec_all_ne(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_all_ne(u8a, u8b))
    i = 1;
}

void
test_vec_any_eq(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_any_eq(u8a, u8b))
    i = 1;
}

void
test_vec_any_ne(vector unsigned char u8a, vector unsigned char u8b)
{
  if (vec_any_ne(u8a, u8b))
    i = 1;
}

void
test_not_vec_all_gt(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_all_gt(u8a, u8b))
    i = 1;
}

void
test_not_vec_all_le(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_all_le(u8a, u8b))
    i = 1;
}

void
test_not_vec_any_gt(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_any_gt(u8a, u8b))
    i = 1;
}

void
test_not_vec_any_le(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_any_le(u8a, u8b))
    i = 1;
}

void
test_not_vec_all_lt(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_all_lt(u8a, u8b))
    i = 1;
}

void
test_not_vec_all_ge(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_all_ge(u8a, u8b))
    i = 1;
}

void
test_not_vec_any_lt(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_any_lt(u8a, u8b))
    i = 1;
}

void
test_not_vec_any_ge(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_any_ge(u8a, u8b))
    i = 1;
}

void
test_not_vec_all_eq(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_all_eq(u8a, u8b))
    i = 1;
}

void
test_not_vec_all_ne(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_all_ne(u8a, u8b))
    i = 1;
}

void
test_not_vec_any_eq(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_any_eq(u8a, u8b))
    i = 1;
}

void
test_not_vec_any_ne(vector unsigned char u8a, vector unsigned char u8b)
{
  if (!vec_any_ne(u8a, u8b))
    i = 1;
}
