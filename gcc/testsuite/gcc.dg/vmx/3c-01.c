#include "harness.h"

vector unsigned char u8;
vector signed char s8;
vector bool char b8;
vector unsigned short u16;
vector signed short s16;
vector bool short b16;
vector unsigned int u32;
vector signed int s32;
vector bool int b32;
vector float f32;
vector pixel p16;

static void g(void);

static void f(void *p)
{
  u8 = vec_ld(16, (unsigned char *)p);
  u16 = vec_ld(16, (unsigned short*)p);
  u32 = vec_ld(16, (unsigned int*)p);
  s8 = vec_ld(16, (signed char *)p);
  s16 = vec_ld(16, (short*)p);
  s32 = vec_ld(16, (int*)p);
  g();
  u8 = vec_ld(16, (vector unsigned char*)p);
  s8 = vec_ld(16, (vector signed char*)p);
  b8 = vec_ld(16, (vector bool char*)p);
  g();
  u16 = vec_ld(16, (vector unsigned short*)p);
  s16 = vec_ld(16, (vector signed short*)p);
  b16 = vec_ld(16, (vector bool short*)p);
  g();
  u32 = vec_ld(16, (vector unsigned int*)p);
  s32 = vec_ld(16, (vector signed int*)p);
  b32 = vec_ld(16, (vector bool int*)p);
  f32 = vec_ld(16, (vector float*)p);
  p16 = vec_ld(16, (vector pixel*)p);
  g();
  u8 = vec_lde(16, (unsigned char *)p);
  u16 = vec_lde(16, (unsigned short*)p);
  u32 = vec_lde(16, (unsigned int*)p);
  s8 = vec_lde(16, (signed char *)p);
  s16 = vec_lde(16, (short*)p);
  s32 = vec_lde(16, (int*)p);
  f32 = vec_ldl(16, (vector float*)p);
  p16 = vec_ldl(16, (vector pixel*)p);
  g();
  u8 = vec_ldl(16, (vector unsigned char*)p);
  s8 = vec_ldl(16, (vector signed char*)p);
  b8 = vec_ldl(16, (vector bool char*)p);
  g();
  u16 = vec_ldl(16, (vector unsigned short*)p);
  s16 = vec_ldl(16, (vector signed short*)p);
  b16 = vec_ldl(16, (vector bool short*)p);
  g();
  u32 = vec_ldl(16, (vector unsigned int*)p);
  s32 = vec_ldl(16, (vector signed int*)p);
  b32 = vec_ldl(16, (vector bool int*)p);
  f32 = vec_ldl(16, (vector float*)p);
  p16 = vec_ldl(16, (vector pixel*)p);
}

static void g () 
{
}

static void test()
{
  static vector unsigned int value = {1,-2,3,-4};
  static vector unsigned int buffer[2];
#define chek(v, s) check(vec_all_eq(v, value), s)
  buffer[1] = value;
  f((void *)buffer);
  chek((vector unsigned int) u8, "u8");
  chek((vector unsigned int) s8, "s8");
  chek((vector unsigned int) b8, "b8");
  chek((vector unsigned int) u16, "u16");
  chek((vector unsigned int) s16, "s16");
  chek((vector unsigned int) b16, "b16");
  chek((vector unsigned int) u32, "u32");
  chek((vector unsigned int) s32, "s32");
  chek((vector unsigned int) b32, "b32");
  chek((vector unsigned int) f32, "f32");
  chek((vector unsigned int) p16, "p16");
}
