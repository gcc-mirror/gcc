/* PR middle-end/97956 - ICE due to type mismatch in pointer_plus_expr
   during memchr folding
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

typedef __INT8_TYPE__  int8_t;
typedef __INT32_TYPE__ int32_t;

extern void* memchr (const void*, int, long);

struct SX
{
  int32_t n;
  int8_t a[];
};

const struct SX sx = { 0x1221 };
const char sx_rep[] = { };

void test_find (void)
{
  int n = 0, nb = (const char*)&sx.a - (const char*)&sx;
  const char *p = (const char*)&sx, *q = sx_rep;
  n += p + 1 == memchr (p, q[1], nb);
}
