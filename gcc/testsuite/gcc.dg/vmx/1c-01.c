#include "harness.h"

/* Declare vector types.  */
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

vector unsigned char *u8c = &u8;
vector signed char *s8c = &s8;
vector bool char *b8c = &b8;
vector unsigned short *u16c = &u16;
vector signed short *s16c = &s16;
vector bool short *b16c = &b16;
vector unsigned int *u32c = &u32;
vector signed int *s32c = &s32;
vector bool int *b32c = &b32;
vector float *f32c = &f32;
vector pixel *p16c = &p16;

static void test()
{
  check(((int)u8c & 15) == 0, "alignof(u8)");
  check(((int)u8c & 15) == 0, "alignof(u8)");
  check(((int)s8c & 15) == 0, "alignof(s8)");
  check(((int)b8c & 15) == 0, "alignof(b8)");
  check(((int)u16c & 15) == 0, "alignof(u16)");
  check(((int)s16c & 15) == 0, "alignof(s16)");
  check(((int)b16c & 15) == 0, "alignof(b16)");
  check(((int)u32c & 15) == 0, "alignof(u32)");
  check(((int)s32c & 15) == 0, "alignof(s32)");
  check(((int)b32c & 15) == 0, "alignof(b32)");
  check(((int)f32c & 15) == 0, "alignof(f32)");
  check(((int)p16c & 15) == 0, "alignof(p16)");

  check((int)u8c == (int)&u8, "u8c == &u8");
  check((int)u8c == (int)&u8, "u8c == &u8");
  check((int)s8c == (int)&s8, "s8c == &s8");
  check((int)b8c == (int)&b8, "b8c == &b8");
  check((int)u16c == (int)&u16, "u16c == &u16");
  check((int)s16c == (int)&s16, "s16c == &s16");
  check((int)b16c == (int)&b16, "b16c == &b16");
  check((int)u32c == (int)&u32, "u32c == &u32");
  check((int)s32c == (int)&s32, "s32c == &s32");
  check((int)b32c == (int)&b32, "b32c == &b32");
  check((int)f32c == (int)&f32, "f32c == &f32");
  check((int)p16c == (int)&p16, "p16c == &p16");
}
