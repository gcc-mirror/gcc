#include "harness.h"

/* Vector types used in aggregates.  */
struct { char b; vector unsigned char a; char e; } u8;
struct { char b; vector signed char a; char e; } s8;
struct { char b; vector bool char a; char e; } b8;
struct { char b; vector unsigned short a; char e; } u16;
struct { char b; vector signed short a; char e; } s16;
struct { char b; vector bool short a; char e; } b16;
struct { char b; vector unsigned int a; char e; } u32;
struct { char b; vector signed int a; char e; } s32;
struct { char b; vector bool int a; char e; } b32;
struct { char b; vector float a; char e; } f32;
struct { char b; vector pixel a; char e; } p16;

union { char b; vector unsigned char a; } u8u;
union { char b; vector signed char a; } s8u;
union { char b; vector bool char a; } b8u;
union { char b; vector unsigned short a; } u16u;
union { char b; vector signed short a; } s16u;
union { char b; vector bool short a; } b16u;
union { char b; vector unsigned int a; } u32u;
union { char b; vector signed int a; } s32u;
union { char b; vector bool int a; } b32u;
union { char b; vector float a; } f32u;
union { char b; vector pixel a; } p16u;

static void test()
{
  check((int)&u8.a - (int)&u8 == 16, "u8.a");
  check((int)&u8.e - (int)&u8 == 32, "u8.e");
  check(sizeof(u8) == 48, "sizeof(u8)");
  check(sizeof(u8u) == 16, "sizeof(u8u)");
}
