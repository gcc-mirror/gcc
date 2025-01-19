/* { dg-do run } */

#define NI __attribute__((noipa))

typedef __INT8_TYPE__ s8;
typedef __INT16_TYPE__ s16;
__extension__ typedef __int24 s24;
typedef __INT32_TYPE__ s32;

static s8 arr8[3];
static s16 arr16[3];
static s24 arr24[3];
static s32 arr32[3];

NI void set8  (s8  *p) { p[0] = -123; p[1]  = -23; p[2]  = -34; }
NI void set16 (s16 *p) { p[0] = -123; p[1] = -234; p[2] = -345; }
NI void set24 (s24 *p) { p[0] = -123; p[1] = -234; p[2] = -345; }
NI void set32 (s32 *p) { p[0] = -123; p[1] = -234; p[2] = -345; }

void test8 (void)
{
  set8 (arr8);
  if (arr8[0] != -123 || arr8[1] != -23 || arr8[2] != -34)
    __builtin_abort();
}

void test16 (void)
{
  set16 (arr16);
  if (arr16[0] != -123 || arr16[1] != -234 || arr16[2] != -345)
    __builtin_abort();
}

void test24 (void)
{
  set24 (arr24);
  if (arr24[0] != -123 || arr24[1] != -234 || arr24[2] != -345)
    __builtin_abort();
}

void test32 (void)
{
  set32 (arr32);
  if (arr32[0] != -123 || arr32[1] != -234 || arr32[2] != -345)
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
