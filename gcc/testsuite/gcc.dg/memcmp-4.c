/* PR middle-end/78257 - missing memcmp optimization with constant arrays
   { dg-do compile }
   { dg-options "-O -Wall -fdump-tree-optimized" } */

typedef __INT8_TYPE__  int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __SIZE_TYPE__  size_t;

extern int memcmp (const void*, const void*, size_t);

/* Verify that initializers for flexible array members are handled
   correctly.  */

struct Si16_x
{
  int16_t n, a[];
};

const struct Si16_x si16_4 =
  {
   0x1112, { 0x2122, 0x3132, 0x4142 }
  };

const char si16_4_rep[] =
  {
   0x12, 0x11, 0x22, 0x21, 0x32, 0x31, 0x42, 0x41
  };

void eq_si16_x (void)
{
  int n = 0, b = sizeof si16_4_rep;
  const char *p = (const char*)&si16_4, *q = si16_4_rep;

  n += memcmp (p,      q,      b);
  n += memcmp (p + 1,  q + 1,  b - 1);
  n += memcmp (p + 2,  q + 2,  b - 2);
  n += memcmp (p + 3,  q + 3,  b - 3);
  n += memcmp (p + 4,  q + 4,  b - 4);
  n += memcmp (p + 5,  q + 5,  b - 5);
  n += memcmp (p + 6,  q + 6,  b - 6);
  n += memcmp (p + 7,  q + 7,  b - 7);
  n += memcmp (p + 8,  q + 8,  b - 8);

  p = (const char*)&si16_4.n;

  n += memcmp (p,      q,          b);
  n += memcmp (p + 1,  q + 1,  b - 1);
  n += memcmp (p + 2,  q + 2,  b - 2);
  n += memcmp (p + 3,  q + 3,  b - 3);
  n += memcmp (p + 4,  q + 4,  b - 4);
  n += memcmp (p + 5,  q + 5,  b - 5);
  n += memcmp (p + 6,  q + 6,  b - 6);
  n += memcmp (p + 7,  q + 7,  b - 7);
  n += memcmp (p + 8,  q + 8,  b - 8);

  p = (const char*)si16_4.a;
  q = si16_4_rep + 2;

  n += memcmp (p,      q,      b - 2);
  n += memcmp (p + 1,  q + 1,  b - 3);
  n += memcmp (p + 2,  q + 2,  b - 4);
  n += memcmp (p + 3,  q + 3,  b - 5);
  n += memcmp (p + 4,  q + 4,  b - 6);
  n += memcmp (p + 5,  q + 5,  b - 7);
  n += memcmp (p + 6,  q + 6,  b - 8);

  p = (const char*)&si16_4.a[1];
  q = si16_4_rep + 4;

  n += memcmp (p,      q,      b - 4);
  n += memcmp (p + 1,  q + 1,  b - 5);
  n += memcmp (p + 2,  q + 2,  b - 6);
  n += memcmp (p + 3,  q + 3,  b - 7);
  n += memcmp (p + 4,  q + 4,  b - 8);

  if (n != 0)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
