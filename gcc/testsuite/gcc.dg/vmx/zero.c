#include "harness.h"

vector signed short zs16() { return ((vector signed short){0,0,0,0,0,0,0,0}); }
vector signed short s16ss() { vector signed short a; return vec_subs(a,a); }
vector signed short s16s() { vector signed short a; return vec_sub(a,a); }
vector signed short s16x() { vector signed short a; return vec_xor(a,a); }
vector signed short s16a() { vector signed short a; return vec_andc(a,a); }

vector unsigned short zu16() { return ((vector unsigned short){0,0,0,0,0,0,0,0}); }
vector unsigned short u16ss() { vector unsigned short a; return vec_subs(a,a); }
vector unsigned short u16s() { vector unsigned short a; return vec_sub(a,a); }
vector unsigned short u16x() { vector unsigned short a; return vec_xor(a,a); }
vector unsigned short u16a() { vector unsigned short a; return vec_andc(a,a); }

vector signed int zs32() { return ((vector signed int){0,0,0,0}); }
vector signed int s32ss() { vector signed int a; return vec_subs(a,a); }
vector signed int s32s() { vector signed int a; return vec_sub(a,a); }
vector signed int s32x() { vector signed int a; return vec_xor(a,a); }
vector signed int s32a() { vector signed int a; return vec_andc(a,a); }

vector unsigned int zu32() { return ((vector unsigned int){0,0,0,0}); }
vector unsigned int u32ss() { vector unsigned int a; return vec_subs(a,a); }
vector unsigned int u32s() { vector unsigned int a; return vec_sub(a,a); }
vector unsigned int u32x() { vector unsigned int a; return vec_xor(a,a); }
vector unsigned int u32a() { vector unsigned int a; return vec_andc(a,a); }

vector signed char zs8() { return ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); }
vector signed char s8ss() { vector signed char a; return vec_subs(a,a); }
vector signed char s8s() { vector signed char a; return vec_sub(a,a); }
vector signed char s8x() { vector signed char a; return vec_xor(a,a); }
vector signed char s8a() { vector signed char a; return vec_andc(a,a); }

vector unsigned char zu8() { return ((vector unsigned char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); }
vector unsigned char u8ss() { vector unsigned char a; return vec_subs(a,a); }
vector unsigned char u8s() { vector unsigned char a; return vec_sub(a,a); }
vector unsigned char u8x() { vector unsigned char a; return vec_xor(a,a); }
vector unsigned char u8a() { vector unsigned char a; return vec_andc(a,a); }

vector pixel zp16() { return ((vector pixel){0,0,0,0,0,0,0,0}); }

vector bool short zb16() { return ((vector bool short){0,0,0,0,0,0,0,0}); }

vector bool short b16x() { vector bool short a; return vec_xor(a,a); }
vector bool short b16a() { vector bool short a; return vec_andc(a,a); }
vector bool int zb32() { return ((vector bool int){0,0,0,0}); }

vector bool int b32x() { vector bool int a; return vec_xor(a,a); }
vector bool int b32a() { vector bool int a; return vec_andc(a,a); }
vector bool char zb8() { return ((vector bool char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}); }

vector bool char b8x() { vector bool char a; return vec_xor(a,a); }
vector bool char b8a() { vector bool char a; return vec_andc(a,a); }

static void test()
{
  static vector unsigned int zerov;
#define zcheck(val, tag) \
  check(vec_all_eq((vector unsigned int)(val), zerov), tag)
  
  zcheck(zs16(), "zs16");
  zcheck(s16ss(), "s16ss");
  zcheck(s16s(), "s16s");
  zcheck(s16x(), "s16x");
  zcheck(s16a(), "s16a");
  zcheck(zu16(), "zu16");
  zcheck(u16ss(), "u16ss");
  zcheck(u16s(), "u16s");
  zcheck(u16x(), "u16x");
  zcheck(u16a(), "u16a");
  zcheck(zs32(), "zs32");
  zcheck(s32ss(), "s32ss");
  zcheck(s32s(), "s32s");
  zcheck(s32x(), "s32x");
  zcheck(s32a(), "s32a");
  zcheck(zu32(), "zu32");
  zcheck(u32ss(), "u32ss");
  zcheck(u32s(), "u32s");
  zcheck(u32x(), "u32x");
  zcheck(u32a(), "u32a");
  zcheck(zs8(), "zs8");
  zcheck(s8ss(), "s8ss");
  zcheck(s8s(), "s8s");
  zcheck(s8x(), "s8x");
  zcheck(s8a(), "s8a");
  zcheck(zu8(), "zu8");
  zcheck(u8ss(), "u8ss");
  zcheck(u8s(), "u8s");
  zcheck(u8x(), "u8x");
  zcheck(u8a(), "u8a");
  zcheck(zp16(), "zp16");
  zcheck(zb16(), "zb16");
  zcheck(b16x(), "b16x");
  zcheck(b16a(), "b16a");
  zcheck(zb32(), "zb32");
  zcheck(b32x(), "b32x");
  zcheck(b32a(), "b32a");
  zcheck(zb8(), "zb8");
  zcheck(b8x(), "b8x");
  zcheck(b8a(), "b8a");
}
