#include <altivec.h>
extern vector unsigned char u8(void);
extern vector signed char s8(void);
extern vector bool char b8(void);
extern vector unsigned short u16(void);
extern vector signed short s16(void);
extern vector bool short b16(void);
extern vector unsigned int u32(void);
extern vector signed int s32(void);
extern vector bool int b32(void);
extern vector float f32(void);
extern vector pixel p16(void);

extern void g(vector unsigned char, ...);

void
f(vector unsigned char u8p, vector signed char s8p, vector bool char b8p,
  vector unsigned short u16p, vector signed short s16p,
  vector bool short b16p, vector unsigned int u32p, 
  vector signed int s32p, vector bool int b32p,
  vector float f32p, vector pixel p16p)
{
  vector unsigned char u8l = u8();
  vector signed char s8l = s8();
  vector bool char b8l = b8();
  vector unsigned short u16l = u16();
  vector signed short s16l = s16();
  vector bool short b16l = b16();
  vector unsigned int u32l = u32();
  vector signed int s32l = s32();
  vector bool int b32l = b32();
  vector float f32l = f32();
  vector pixel p16l = p16();

  g(u8l, s8l, b8l, u16l, s16l, b16l, u32l, s32l, b32l, f32l, p16l);
}

vector unsigned char
u8(void)
{
  static vector unsigned char zero;
  return zero;
}

vector signed char
s8(void)
{
  static vector signed char zero;
  return zero;
}

vector bool char
b8(void)
{
  static vector bool char zero;
  return zero;
}

vector unsigned short
u16(void)
{
  static vector unsigned short zero;
  return zero;
}

vector signed short
s16(void)
{
  static vector signed short zero;
  return zero;
}

vector bool short
b16(void)
{
  static vector bool short zero;
  return zero;
}

vector unsigned int
u32(void)
{
  static vector unsigned int zero;
  return zero;
}

vector signed int
s32(void)
{
  static vector signed int zero;
  return zero;
}

vector bool int
b32(void)
{
  static vector bool int zero;
  return zero;
}

vector float
f32(void)
{
  static vector float zero;
  return zero;
}

vector pixel
p16(void)
{
  static vector pixel zero;
  return zero;
}

void
g(vector unsigned char a, ...)
{
}

int main()
{
  f(u8(), s8(), b8(), u16(), s16(), b16(), u32(), s32(), b32(), f32(), p16());
  return 0; 
}
