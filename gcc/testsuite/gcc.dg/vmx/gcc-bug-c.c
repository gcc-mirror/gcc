/* { dg-do compile } */
#include <altivec.h>
vector signed short s16ss() { vector signed short a; return vec_subs(a,a); }
vector signed short s16s() { vector signed short a; return vec_sub(a,a); }
vector signed short s16x() { vector signed short a; return vec_xor(a,a); }
vector signed short s16a() { vector signed short a; return vec_andc(a,a); }
vector unsigned char u8;
vector signed short s16;
vector bool int b32;
vector float f32;
vector pixel p16;
void x()
{
  u8 = ((vector unsigned char){3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3});
  s16 = ((vector signed short){-7,-7,-7,-7,-7,-7,-7,-7});
  b32 = ((vector bool int) {10,20,30,40});
  f32 = ((vector float) {2,4,6,8});
  p16 = ((vector pixel){23,23,23,23,23,23,23,23});
}
vector unsigned int a;
vector unsigned int b;
void f(void) {
  a = ((vector unsigned int){1,1,1,1});
  b = ((vector unsigned int){1,2,3,4});
}
