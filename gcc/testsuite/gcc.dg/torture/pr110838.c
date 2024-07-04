/* { dg-do run } */

typedef __UINT32_TYPE__ uint32_t;
typedef __UINT8_TYPE__ uint8_t;
typedef __INT8_TYPE__ int8_t;
typedef uint8_t pixel;

#define MSB (__CHAR_BIT__ * __SIZEOF_INT__ - 1)

/* get the sign of input variable (TODO: this is a dup, make common) */
static inline int8_t signOf(int x)
{
  return (x >> MSB) | ((int)((((uint32_t)-x)) >> MSB));
}

__attribute__((noipa))
static void calSign_bug(int8_t *dst, const pixel *src1, const pixel *src2, const int endX)
{
  for (int x = 0; x < endX; x++)
    dst[x] = signOf(src1[x] - src2[x]);
}

__attribute__((noipa, optimize(0)))
static void calSign_ok(int8_t *dst, const pixel *src1, const pixel *src2, const int endX)
{
  for (int x = 0; x < endX; x++)
    dst[x] = signOf(src1[x] - src2[x]);
}

__attribute__((noipa, optimize(0)))
int main()
{
  const pixel s1[9] = { 0xcd, 0x33, 0xd4, 0x3e, 0xb0, 0xfb, 0x95, 0x64, 0x70, };
  const pixel s2[9] = { 0xba, 0x9f, 0xab, 0xa1, 0x3b, 0x29, 0xb1, 0xbd, 0x64, };
  int endX = 9;
  int8_t dst[9];
  int8_t dst_ok[9];

  calSign_bug(dst, s1, s2, endX);
  calSign_ok(dst_ok, s1, s2, endX);

  if (__builtin_memcmp(dst, dst_ok, endX) != 0)
    __builtin_abort ();
  return 0;
}
