#include <altivec.h>
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

void f_u8(vector unsigned char *p) {
  u8 = vec_add(*p, *p);
}
void f_s8(vector signed char *p) {
  s8 = vec_add(*p, *p);
}
void f_b8(vector bool char *p) {
  b8 = vec_cmpgt(s8, s8);
  b8 = vec_xor(b8, *p);
}
void f_u16(vector unsigned short *p) {
  u16 = vec_add(*p, *p);
}
void f_s16(vector signed short *p) {
  s16 = vec_add(*p, *p);
}
void f_b16(vector bool short *p) {
  b16 = vec_cmpgt(s16, s16);
  b16 = vec_xor(b16, *p);
}
void f_u32(vector unsigned int *p) {
  u32 = vec_add(*p, *p);
}
void f_s32(vector signed int *p) {
  s32 = vec_add(*p, *p);
}
void f_b32(vector bool int *p) {
  b32 = vec_cmpgt(s32, s32);
  b32 = vec_xor(b32, *p);
}
void f_f32(vector float *p) {
  f32 = vec_add(*p, *p);
}
void f_p16(vector pixel *p) {
  p16 = *p;
}

int main() {
  vector unsigned char u8 = ((vector unsigned char){1, 2, 3, 4, 5, 6, 7, 8,
						   9, 10,11,12,13,14,15,16});
  vector signed char s8 = ((vector signed char){1, 2, 3, 4, 5, 6, 7, 8,
					       9, 10,11,12,13,14,15,16});
  vector bool char b8 = ((vector bool char){0, -1, 0, -1, 0, 0, 0, 0,
					   -1, -1, -1, -1, 0, -1, 0, -1});
  vector unsigned short u16 = ((vector unsigned short){1, 2, 3, 4, 5, 6, 7, 8});
  vector signed short s16 = ((vector signed short){1, 2, 3, 4, 5, 6, 7, 8});
  vector bool short b16 = ((vector bool short){-1, 0, -1, 0, -1, -1, 0, 0});
  vector unsigned int u32 = ((vector unsigned int){1, 2, 3, 4});
  vector signed int s32 = ((vector signed int){1, 2, 3, 4});
  vector bool int b32 = ((vector bool int){0, -1, -1, 0});
  vector float f32 = ((vector float){1, 2, 3, 4});
  vector pixel p16 = ((vector pixel){1, 2, 3, 4, 5, 6, 7, 8});
  f_u8(&u8);
  f_s8(&s8);
  f_b8(&b8);
  f_u16(&u16);
  f_s16(&s16);
  f_b16(&b16);
  f_u32(&u32);
  f_s32(&s32);
  f_b32(&b32);
  f_f32(&f32);
  f_p16(&p16);
  return 0;
}
