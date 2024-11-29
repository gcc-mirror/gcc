// { dg-do compile }
// { dg-require-effective-target vect_int }
// { dg-require-effective-target vect_unpack }

// PR tree-optimization/117776

typedef __SIZE_TYPE__ size_t;
typedef unsigned int u32;
typedef unsigned char u8;

static inline const bool
is_even_bool(u8 n)
{
 return n % 2 == 0;
}

static inline const u32
is_even_u32(u8 n)
{
  return n % 2 == 0;
}

static inline
const u32 is_even_convert(u8 n)
{
  return is_even_bool(n);
}

u32 count_even_v1(u8 *data, size_t len)
{
  u32 ret = 0;
  for (size_t i = 0; i < len; i++)
    ret += is_even_bool(data[i]);
  return ret;
}

u32 count_even_v2(u8 *data, size_t len)
{
  u32 ret = 0;
  for (size_t i = 0; i < len; i++)
    ret += is_even_u32(data[i]);
  return ret;
}

u32 count_even_v3(u8 *data, size_t len)
{
  u32 ret = 0;
  for (size_t i = 0; i < len; i++)
    ret += is_even_convert(data[i]);
  return ret;
}

u32 count_even_v4(u8 *data, size_t len)
{
  u32 ret = 0;
  for (size_t i = 0; i < len; i++)
    ret += static_cast<u32>(is_even_bool(data[i]));
  return ret;
}

// All 4 count_even_v functions should be vectorized

// { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 4 "vect" } }
