/* { dg-do run } */

#define NI __attribute__((noinline, noclone))

typedef __INT8_TYPE__ s8;
typedef __INT16_TYPE__ s16;
typedef __int24 s24;
typedef __INT32_TYPE__ s32;

static const s8 arr8[] = { 12, 23, 34 };
static const s16 arr16[] = { 123, 234, 345 };
static const s24 arr24[] = { 1234, 2345, 3456 };
static const s32 arr32[] = { 12345, 23456, 34567 };

NI s8  add8  (const s8  *p) { return p[0] + p[1] + p[2]; }
NI s16 add16 (const s16 *p) { return p[0] + p[1] + p[2]; }
NI s24 add24 (const s24 *p) { return p[0] + p[1] + p[2]; }
NI s32 add32 (const s32 *p) { return p[0] + p[1] + p[2]; }

void test8 (void)
{
  if (add8 (arr8) != arr8[0] + arr8[1] + arr8[2])
    __builtin_abort();
}

void test16 (void)
{
  if (add16 (arr16) != arr16[0] + arr16[1] + arr16[2])
    __builtin_abort();
}

void test24 (void)
{
  if (add24 (arr24) != arr24[0] + arr24[1] + arr24[2])
    __builtin_abort();
}

void test32 (void)
{
  if (add32 (arr32) != arr32[0] + arr32[1] + arr32[2])
    __builtin_abort();
}

int main (void)
{
  test8();
  test16();
  test24();
  test32();

  return 0;
}
