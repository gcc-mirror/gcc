/* PR middle-end/78257 - missing memcmp optimization with constant arrays
   { dg-do compile }
   { dg-options "-O -Wall -fdump-tree-optimized" }
   { dg-skip-if "missing data representation" { "pdp11-*-*" } }
   { dg-skip-if "test assumes structs are not packed" { default_packed } } */

#define offsetof(T, m) __builtin_offsetof (T, m)

typedef __INT8_TYPE__  int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
typedef __SIZE_TYPE__  size_t;

extern int memcmp (const void*, const void*, size_t);

const int32_t ia4[4] = { 0x11121314, 0x21222324, 0x31323334, 0x41424344 };
const int32_t ia4_des[4] =
  { [2] = 0x31323334, [0] = 0x11121314, 0x21222324, [3] = 0x41424344 };
const char ia4_rep[] =
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
   "\x11\x12\x13\x14" "\x21\x22\x23\x24"
   "\x31\x32\x33\x34" "\x41\x42\x43\x44"
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
   "\x14\x13\x12\x11" "\x24\x23\x22\x21"
   "\x34\x33\x32\x31" "\x44\x43\x42\x41"
#endif
  };

void eq_ia4 (void)
{
  int n = 0, b = sizeof ia4;
  const char *p = (const char*)ia4, *q = ia4_rep;

  n += memcmp (p,      q,      b);
  n += memcmp (p + 1,  q + 1,  b - 1);
  n += memcmp (p + 2,  q + 2,  b - 2);
  n += memcmp (p + 3,  q + 3,  b - 3);
  n += memcmp (p + 4,  q + 4,  b - 4);
  n += memcmp (p + 5,  q + 5,  b - 5);
  n += memcmp (p + 6,  q + 6,  b - 6);
  n += memcmp (p + 7,  q + 7,  b - 7);
  n += memcmp (p + 8,  q + 8,  b - 8);
  n += memcmp (p + 9,  q + 9,  b - 9);
  n += memcmp (p + 10, q + 10, b - 10);
  n += memcmp (p + 11, q + 11, b - 11);
  n += memcmp (p + 12, q + 12, b - 12);
  n += memcmp (p + 13, q + 13, b - 13);
  n += memcmp (p + 14, q + 14, b - 14);
  n += memcmp (p + 15, q + 15, b - 15);
  n += memcmp (p + 16, q + 16, b - 16);

  p = (const char*)ia4_des;

  n += memcmp (p,      q,      b);
  n += memcmp (p + 1,  q + 1,  b - 1);
  n += memcmp (p + 2,  q + 2,  b - 2);
  n += memcmp (p + 3,  q + 3,  b - 3);
  n += memcmp (p + 4,  q + 4,  b - 4);
  n += memcmp (p + 5,  q + 5,  b - 5);
  n += memcmp (p + 6,  q + 6,  b - 6);
  n += memcmp (p + 7,  q + 7,  b - 7);
  n += memcmp (p + 8,  q + 8,  b - 8);
  n += memcmp (p + 9,  q + 9,  b - 9);
  n += memcmp (p + 10, q + 10, b - 10);
  n += memcmp (p + 11, q + 11, b - 11);
  n += memcmp (p + 12, q + 12, b - 12);
  n += memcmp (p + 13, q + 13, b - 13);
  n += memcmp (p + 14, q + 14, b - 14);
  n += memcmp (p + 15, q + 15, b - 15);
  n += memcmp (p + 16, q + 16, b - 16);

  if (n != 0)
    __builtin_abort ();
}

const float fa4[4] = { 1.0, 2.0, 3.0, 4.0 };
const float fa4_des[4] = { [0] = fa4[0], [1] = 2.0, [2] = fa4[2], [3] = 4.0 };

void eq_fa4 (void)
{
  int n = 0, b = sizeof fa4;
  const char *p = (const char*)fa4, *q = (const char*)fa4_des;

  n += memcmp (p,      q,      b);
  n += memcmp (p + 1,  q + 1,  b - 1);
  n += memcmp (p + 2,  q + 2,  b - 2);
  n += memcmp (p + 3,  q + 3,  b - 3);
  n += memcmp (p + 4,  q + 4,  b - 4);
  n += memcmp (p + 5,  q + 5,  b - 5);
  n += memcmp (p + 6,  q + 6,  b - 6);
  n += memcmp (p + 7,  q + 7,  b - 7);
  n += memcmp (p + 8,  q + 8,  b - 8);
  n += memcmp (p + 9,  q + 9,  b - 9);
  n += memcmp (p + 10, q + 10, b - 10);
  n += memcmp (p + 11, q + 11, b - 11);
  n += memcmp (p + 12, q + 12, b - 12);
  n += memcmp (p + 13, q + 13, b - 13);
  n += memcmp (p + 14, q + 14, b - 14);
  n += memcmp (p + 15, q + 15, b - 15);
  n += memcmp (p + 16, q + 16, b - 16);

  if (n != 0)
    __builtin_abort ();
}

/* Verify "greater than" comparison with the difference in the last byte.  */
const char ia4_xrep_16[sizeof ia4] =
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
   0x11, 0x12, 0x13, 0x14, 0x21, 0x22, 0x23, 0x24,
   0x31, 0x32, 0x33, 0x34, 0x41, 0x42, 0x43
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
   0x14, 0x13, 0x12, 0x11, 0x24, 0x23, 0x22, 0x21,
   0x34, 0x33, 0x32, 0x31, 0x44, 0x43, 0x42
#endif
  };

void gt_ia4 (void)
{
  int n = 0, b = sizeof ia4;
  const char *p = (const char*)ia4, *q = ia4_xrep_16;

  n += 0 < memcmp (p,      q,      b);
  n += 0 < memcmp (p + 1,  q + 1,  b - 1);
  n += 0 < memcmp (p + 2,  q + 2,  b - 2);
  n += 0 < memcmp (p + 3,  q + 3,  b - 3);
  n += 0 < memcmp (p + 4,  q + 4,  b - 4);
  n += 0 < memcmp (p + 5,  q + 5,  b - 5);
  n += 0 < memcmp (p + 6,  q + 6,  b - 6);
  n += 0 < memcmp (p + 7,  q + 7,  b - 7);
  n += 0 < memcmp (p + 8,  q + 8,  b - 8);
  n += 0 < memcmp (p + 9,  q + 9,  b - 9);
  n += 0 < memcmp (p + 10, q + 10, b - 10);
  n += 0 < memcmp (p + 11, q + 11, b - 11);
  n += 0 < memcmp (p + 12, q + 12, b - 12);
  n += 0 < memcmp (p + 13, q + 13, b - 13);
  n += 0 < memcmp (p + 14, q + 14, b - 14);
  n += 0 < memcmp (p + 15, q + 15, b - 15);

  if (n != 16)
    __builtin_abort ();
}

struct S8_16_32
{
  int8_t  i8;
  int16_t i16;
  int32_t i32;
};

_Static_assert (sizeof (struct S8_16_32) == 8);

const struct S8_16_32 s8_16_32 = { 1, 0x2122, 0x31323334 };
const struct S8_16_32 s8_16_32_des =
  { .i8 = 1, .i16 = 0x2122, .i32 = 0x31323334 };

const char s8_16_32_rep[] =
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
   1, 0, 0x21, 0x22, 0x31, 0x32, 0x33, 0x34
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
   1, 0, 0x22, 0x21, 0x34, 0x33, 0x32, 0x31
#endif
  };

void eq_s8_16_32 (void)
{
  int n = 0, b = sizeof s8_16_32;
  const char *p = (char*)&s8_16_32, *q = s8_16_32_rep;

  n += memcmp (p,     q,     b);
  n += memcmp (p + 1, q + 1, b - 1);
  n += memcmp (p + 2, q + 2, b - 2);
  n += memcmp (p + 3, q + 3, b - 3);
  n += memcmp (p + 4, q + 4, b - 4);
  n += memcmp (p + 5, q + 5, b - 5);
  n += memcmp (p + 6, q + 6, b - 6);
  n += memcmp (p + 7, q + 7, b - 7);

  p = (char*)&s8_16_32_des;

  n += memcmp (p,     q,     b);
  n += memcmp (p + 1, q + 1, b - 1);
  n += memcmp (p + 2, q + 2, b - 2);
  n += memcmp (p + 3, q + 3, b - 3);
  n += memcmp (p + 4, q + 4, b - 4);
  n += memcmp (p + 5, q + 5, b - 5);
  n += memcmp (p + 6, q + 6, b - 6);
  n += memcmp (p + 7, q + 7, b - 7);

  if (n != 0)
    __builtin_abort ();
}


struct S8_16_32_64
{
  /*  0 */ int8_t   i8;
  /*  1 */ int8_t:  1;
  /*  2 */ int16_t  i16;
  /*  4 */ int32_t: 1;
  /*  8 */ int32_t  i32;
  /* 12 */ int32_t: 1;
  /* 16 */ int64_t  i64;
  /* 24 */ int8_t:  0;
};

_Static_assert (offsetof (struct S8_16_32_64, i16) == 2);
_Static_assert (offsetof (struct S8_16_32_64, i32) == 8);
_Static_assert (offsetof (struct S8_16_32_64, i64) == 16);
_Static_assert (sizeof (struct S8_16_32_64) == 24);

const struct S8_16_32_64 s8_16_32_64 =
  { 1, 0x2122, 0x31323334, 0x4142434445464748LLU };

const char s8_16_32_64_rep[sizeof s8_16_32_64] =
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
   "\x01" "\x00" "\x21\x22" "\x00\x00\x00\x00" "\x31\x32\x33\x34"
   "\x00\x00\x00\x00" "\x41\x42\x43\x44\x45\x46\x47\x48"
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
   "\x01" "\x00" "\x22\x21" "\x00\x00\x00\x00" "\x34\x33\x32\x31"
   "\x00\x00\x00\x00" "\x48\x47\x46\x45\x44\x43\x42\x41"
#endif
  };

const struct S8_16_32_64 s8_16_32_64_des =
  { .i64 = 0x4142434445464748LLU, .i16 = 0x2122, .i32 = 0x31323334, .i8 = 1 };


void eq_8_16_32_64 (void)
{
  int n = 0, b = sizeof s8_16_32_64;
  const char *p = (char*)&s8_16_32_64, *q = s8_16_32_64_rep;

  n += memcmp (p, q, b);
  n += memcmp (p + 1,  q + 1,  b - 1);
  n += memcmp (p + 2,  q + 2,  b - 2);
  n += memcmp (p + 3,  q + 3,  b - 3);
  n += memcmp (p + 4,  q + 4,  b - 4);
  n += memcmp (p + 5,  q + 5,  b - 5);
  n += memcmp (p + 6,  q + 6,  b - 6);
  n += memcmp (p + 7,  q + 7,  b - 7);
  n += memcmp (p + 8,  q + 8,  b - 8);
  n += memcmp (p + 9,  q + 9,  b - 9);
  n += memcmp (p + 10, q + 10, b - 10);
  n += memcmp (p + 11, q + 11, b - 11);
  n += memcmp (p + 12, q + 12, b - 12);
  n += memcmp (p + 13, q + 13, b - 13);
  n += memcmp (p + 14, q + 14, b - 14);
  n += memcmp (p + 15, q + 15, b - 15);
  n += memcmp (p + 16, q + 16, b - 16);
  n += memcmp (p + 17, q + 17, b - 17);
  n += memcmp (p + 18, q + 18, b - 18);
  n += memcmp (p + 19, q + 19, b - 19);
  n += memcmp (p + 20, q + 20, b - 20);
  n += memcmp (p + 21, q + 21, b - 21);
  n += memcmp (p + 22, q + 22, b - 22);
  n += memcmp (p + 23, q + 23, b - 23);

  p = (char*)&s8_16_32_64_des;

  n += memcmp (p, q, b);
  n += memcmp (p + 1,  q + 1,  b - 1);
  n += memcmp (p + 2,  q + 2,  b - 2);
  n += memcmp (p + 3,  q + 3,  b - 3);
  n += memcmp (p + 4,  q + 4,  b - 4);
  n += memcmp (p + 5,  q + 5,  b - 5);
  n += memcmp (p + 6,  q + 6,  b - 6);
  n += memcmp (p + 7,  q + 7,  b - 7);
  n += memcmp (p + 8,  q + 8,  b - 8);
  n += memcmp (p + 9,  q + 9,  b - 9);
  n += memcmp (p + 10, q + 10, b - 10);
  n += memcmp (p + 11, q + 11, b - 11);
  n += memcmp (p + 12, q + 12, b - 12);
  n += memcmp (p + 13, q + 13, b - 13);
  n += memcmp (p + 14, q + 14, b - 14);
  n += memcmp (p + 15, q + 15, b - 15);
  n += memcmp (p + 16, q + 16, b - 16);
  n += memcmp (p + 17, q + 17, b - 17);
  n += memcmp (p + 18, q + 18, b - 18);
  n += memcmp (p + 19, q + 19, b - 19);
  n += memcmp (p + 20, q + 20, b - 20);
  n += memcmp (p + 21, q + 21, b - 21);
  n += memcmp (p + 22, q + 22, b - 22);
  n += memcmp (p + 23, q + 23, b - 23);

  if (n != 0)
    __builtin_abort ();
}

struct S64_x_3
{
  int64_t i64a[3];
};

_Static_assert (sizeof (struct S64_x_3) == 24);

const struct S64_x_3 s64_x_3 =
  { { 0x0000000021220001LLU, 0x0000000031323334LLU, 0x4142434445464748LLU } };

const char s64_x_3_rep[sizeof s64_x_3] =
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
   "\x00\x00\x00\x00\x21\x22\x00\x01"
   "\x00\x00\x00\x00\x31\x32\x33\x34"
   "\x41\x42\x43\x44\x45\x46\x47\x48"
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
   "\x01\x00\x22\x21\x00\x00\x00\x00"
   "\x34\x33\x32\x31\x00\x00\x00\x00"
   "\x48\x47\x46\x45\x44\x43\x42\x41"
#endif
  };

void eq_64_x_3 (void)
{
  int n = 0, b = sizeof s8_16_32_64;
  const char *p = (char*)&s8_16_32_64, *q = s64_x_3_rep;
  n += memcmp (p, q, b);
  n += memcmp (p + 1,  q + 1,  b - 1);
  n += memcmp (p + 2,  q + 2,  b - 2);
  n += memcmp (p + 3,  q + 3,  b - 3);
  n += memcmp (p + 4,  q + 4,  b - 4);
  n += memcmp (p + 5,  q + 5,  b - 5);
  n += memcmp (p + 6,  q + 6,  b - 6);
  n += memcmp (p + 7,  q + 7,  b - 7);
  n += memcmp (p + 8,  q + 8,  b - 8);
  n += memcmp (p + 9,  q + 9,  b - 9);
  n += memcmp (p + 10, q + 10, b - 10);
  n += memcmp (p + 11, q + 11, b - 11);
  n += memcmp (p + 12, q + 12, b - 12);
  n += memcmp (p + 13, q + 13, b - 13);
  n += memcmp (p + 14, q + 14, b - 14);
  n += memcmp (p + 15, q + 15, b - 15);
  n += memcmp (p + 16, q + 16, b - 16);
  n += memcmp (p + 17, q + 17, b - 17);
  n += memcmp (p + 18, q + 18, b - 18);
  n += memcmp (p + 19, q + 19, b - 19);
  n += memcmp (p + 20, q + 20, b - 20);
  n += memcmp (p + 21, q + 21, b - 21);
  n += memcmp (p + 22, q + 22, b - 22);
  n += memcmp (p + 23, q + 23, b - 23);

  if (n != 0)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
