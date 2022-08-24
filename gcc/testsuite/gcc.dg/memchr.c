/* PR middle-end/78257 - missing memcmp optimization with constant arrays
   { dg-do compile }
   { dg-options "-O -Wall -fdump-tree-optimized" }
   { dg-skip-if "test assumes structs are not packed" { default_packed } } */

typedef __INT8_TYPE__  int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __SIZE_TYPE__  size_t;

extern void* memchr (const void*, int, size_t);

/* Verify that initializers for flexible array members are handled
   correctly.  */

struct SX
{
  /* offset */
  /*   0    */ int32_t n;
  /*   4    */ int8_t: 1;
  /*   6    */ int16_t a[];
};

_Static_assert (__builtin_offsetof (struct SX, a) == 6);

const struct SX sx =
  {
   0x11121314, { 0x2122, 0x3132, 0x4142, 0x5152 }
  };

const char sx_rep[] =
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
   0x11, 0x12, 0x13, 0x14, 0, 0, 0x21, 0x22, 0x31, 0x32, 0x41, 0x42, 0x51, 0x52
#else
   0x14, 0x13, 0x12, 0x11, 0, 0, 0x22, 0x21, 0x32, 0x31, 0x42, 0x41, 0x52, 0x51
#endif
  };


void test_find (void)
{
  int n = 0, nb = (const char*)&sx.a[4] - (const char*)&sx;
  const char *p = (const char*)&sx, *q = sx_rep;

  if (nb != sizeof sx_rep)
    __builtin_abort ();

  n += p      == memchr (p, q[ 0], nb);
  n += p +  1 == memchr (p, q[ 1], nb);
  n += p +  2 == memchr (p, q[ 2], nb);
  n += p +  3 == memchr (p, q[ 3], nb);
  n += p +  4 == memchr (p, q[ 4], nb);
  n += p +  4 == memchr (p, q[ 5], nb);
  n += p +  6 == memchr (p, q[ 6], nb);
  n += p +  7 == memchr (p, q[ 7], nb);
  n += p +  8 == memchr (p, q[ 8], nb);
  n += p +  9 == memchr (p, q[ 9], nb);
  n += p + 10 == memchr (p, q[10], nb);
  n += p + 11 == memchr (p, q[11], nb);
  n += p + 12 == memchr (p, q[12], nb);
  n += p + 13 == memchr (p, q[13], nb);

  if (n != 14)
    __builtin_abort ();
}

void test_not_find (void)
{
  int n = 0, nb = (const char*)&sx.a[4] - (const char*)&sx;
  const char *p = (const char*)&sx, *q = sx_rep;

  if (nb != sizeof sx_rep)
    __builtin_abort ();

  n += 0 == memchr (p,      0xff, nb);
  n += 0 == memchr (p +  1, q[ 0], nb - 1);
  n += 0 == memchr (p +  2, q[ 1], nb - 2);
  n += 0 == memchr (p +  3, q[ 2], nb - 3);
  n += 0 == memchr (p +  4, q[ 3], nb - 4);
  n += 0 == memchr (p +  6, q[ 4], nb - 6);
  n += 0 == memchr (p +  7, q[ 6], nb - 7);
  n += 0 == memchr (p +  8, q[ 7], nb - 8);
  n += 0 == memchr (p +  9, q[ 8], nb - 9);
  n += 0 == memchr (p + 10, q[ 9], nb - 10);
  n += 0 == memchr (p + 11, q[10], nb - 11);
  n += 0 == memchr (p + 12, q[11], nb - 12);
  n += 0 == memchr (p + 13, q[12], nb - 13);
  n += 0 == memchr (p + 14, q[13], nb - 14);

  if (n != 14)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
