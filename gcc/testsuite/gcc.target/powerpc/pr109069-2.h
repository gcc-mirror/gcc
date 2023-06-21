#include <altivec.h>

typedef union
{
  unsigned int i;
  float f;
} U32b;

typedef union
{
  unsigned long long i;
  double f;
} U64b;

__attribute__ ((noipa))
vector unsigned char
test1 ()
{
  vector unsigned char v = {0xd, 0xd, 0xd, 0xd, 0xd, 0xd, 0xd, 0xd,
			    0xd, 0xd, 0xd, 0xd, 0xd, 0xd, 0xd, 0xd};
  vector unsigned char res = vec_sld (v, v, 3);
  return res;
}

__attribute__ ((noipa))
vector signed short
test2 ()
{
  vector signed short v
    = {0x7777, 0x7777, 0x7777, 0x7777, 0x7777, 0x7777, 0x7777, 0x7777};
  vector signed short res = vec_sld (v, v, 5);
  return res;
}

__attribute__ ((noipa))
vector signed int
test3 ()
{
  vector signed int v = {0xbbbbbbbb, 0xbbbbbbbb, 0xbbbbbbbb, 0xbbbbbbbb};
  vector signed int res = vec_sld (v, v, 7);
  return res;
}

__attribute__ ((noipa))
vector unsigned int
test4 ()
{
  vector unsigned int v = {0x07070707, 0x07070707, 0x07070707, 0x07070707};
  vector unsigned int res = vec_sld (v, v, 9);
  return res;
}

__attribute__ ((noipa))
vector unsigned long long
test5 ()
{
  vector unsigned long long v = {0x4545454545454545ll, 0x4545454545454545ll};
  vector unsigned long long res = vec_sld (v, v, 10);
  return res;
}

__attribute__ ((noipa))
vector float
test6 ()
{
  U32b u;
  u.i = 0x17171717;
  vector float vf = {u.f, u.f, u.f, u.f};
  vector float res = vec_sld (vf, vf, 11);
  return res;
}

__attribute__ ((noipa))
vector double
test7 ()
{
  U64b u;
  u.i = 0x5454545454545454ll;
  vector double vf = {u.f, u.f};
  vector double res = vec_sld (vf, vf, 13);
  return res;
}

