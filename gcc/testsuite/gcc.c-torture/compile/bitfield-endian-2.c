/* { dg-require-effective-target int128 } */

#define ENDIAN __attribute((scalar_storage_order ("little-endian")))

typedef struct ENDIAN
{
  __uint128_t t:124;
  __uint128_t t1:4;
}f;

f g(void)
{
  f t = {1, 2};
  return t;
}
