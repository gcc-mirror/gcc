#include <altivec.h>
vector unsigned char u8 = {1, 2, 3, 4, 5, 6, 7, 8,
			   9, 10,11,12,13,14,15,16};
vector signed char s8 = {1, 2, 3, 4, 5, 6, 7, 8,
			 9, 10,11,12,13,14,15,16};
vector bool char b8 = {0, -1, 0, -1, 0, 0, 0, 0,
		      -1, -1, -1, -1, 0, -1, 0, -1};
vector unsigned short u16 = {1, 2, 3, 4, 5, 6, 7, 8};
vector signed short s16 = {1, 2, 3, 4, 5, 6, 7, 8};
vector bool short b16 = {-1, 0, -1, 0, -1, -1, 0, 0};
vector unsigned int u32 = {1, 2, 3, 4};
vector signed int s32 = {1, 2, 3, 4};
vector bool int b32 = {0, -1, -1, 0};
vector float f32 = {1, 2, 3, 4};
vector pixel p16 = {1, 2, 3, 4, 5, 6, 7, 8};

static void f_u8(vector unsigned char *p) {}
static void f_s8(vector signed char *p) {}
static void f_b8(vector bool char *p) {}
static void f_u16(vector unsigned short *p) {}
static void f_s16(vector signed short *p) {}
static void f_b16(vector bool short *p) {}
static void f_u32(vector unsigned int *p) {}
static void f_s32(vector signed int *p) {}
static void f_b32(vector bool int *p) {}
static void f_f32(vector float *p) {}
static void f_p16(vector pixel *p) {}

int main() {
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
