/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;

#define MAGIC (~ (uint32_t) 0 / 2 + 1)

int
f_i16_i16 (int16_t x, int16_t y)
{
  return x + MAGIC < y + MAGIC;
}

int
f_i16_i32 (int16_t x, int32_t y)
{
  return x + MAGIC < y + MAGIC;
}

int
f_i32_i32 (int32_t x, int32_t y)
{
  return x + MAGIC < y + MAGIC;
}

int
f_u32_i32 (uint32_t x, int32_t y)
{
  return x + MAGIC < y + MAGIC;
}

int
f_u32_u32 (uint32_t x, uint32_t y)
{
  return x + MAGIC < y + MAGIC;
}

int
f_i32_i32_sub (int32_t x, int32_t y)
{
  return x - MAGIC < y - MAGIC;
}

/* The addition/subtraction of constants should be optimized away.  */
/* { dg-final { scan-tree-dump-not " \\+ " "optimized"} } */
/* { dg-final { scan-tree-dump-not " \\- " "optimized"} } */
