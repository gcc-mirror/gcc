/* { dg-do compile } */
#include <altivec.h>
extern void fu32(vector unsigned int, vector unsigned int,
		 vector unsigned int, vector unsigned int);
void fxu32(vector unsigned int u32a, vector unsigned int u32b,
	   vector unsigned int u32c, vector unsigned int u32d)
{
 fu32 (u32a,
       u32b,
       u32c,
       vec_avg(u32d, u32d));
 fu32 (vec_or (u32a, u32a),
       vec_and (u32b, u32b),
       vec_max (u32c, u32c),
       vec_min (u32d, u32d));
 fu32 (vec_sld (u32a, u32a, 0),
       vec_sld (u32b, u32b, 0),
       vec_sld (u32c, u32c, 0),
       vec_sld (u32d, u32d, 0));
 fu32 (((vector unsigned int){0,0,0,0}),
       ((vector unsigned int){0,0,0,0}),
       ((vector unsigned int){0,0,0,0}),
       ((vector unsigned int){0,0,0,0}));
 fu32 (vec_xor(u32a, u32a),
       vec_andc(u32b, u32b),
       vec_sub(u32c, u32c),
       vec_subs(u32d, u32d));
 fu32 (vec_splat_u32(0),
       vec_splat_u32(0),
       vec_splat_u32(0),
       vec_splat_u32(0));
 fu32 (((vector unsigned int){0xffffffff,0xffffffff,0xffffffff,0xffffffff}),
       ((vector unsigned int){0xffffffff,0xffffffff,0xffffffff,0xffffffff}),
       ((vector unsigned int){0xffffffff,0xffffffff,0xffffffff,0xffffffff}),
       ((vector unsigned int){0xffffffff,0xffffffff,0xffffffff,0xffffffff}));
 fu32 (vec_splat_u32(-1),
       vec_splat_u32(-1),
       vec_splat_u32(-1),
       vec_splat_u32(-1));
 fu32 ((vector unsigned int)vec_cmpeq(u32a, u32a),
       (vector unsigned int)vec_cmpeq(u32b, u32b),
       (vector unsigned int)vec_cmpeq(u32c, u32c),
       (vector unsigned int)vec_cmpeq(u32d, u32d));
}

extern void fu16(vector unsigned short, vector unsigned short,
		 vector unsigned short, vector unsigned short);
void fxu16(vector unsigned short u16a, vector unsigned short u16b,
	   vector unsigned short u16c, vector unsigned short u16d)
{
 fu16 (u16a,
       u16b,
       u16c,
       vec_avg(u16d, u16d));
 fu16 (vec_or (u16a, u16a),
       vec_and (u16b, u16b),
       vec_max (u16c, u16c),
       vec_min (u16d, u16d));
 fu16 (vec_sld (u16a, u16a, 0),
       vec_sld (u16b, u16b, 0),
       vec_sld (u16c, u16c, 0),
       vec_sld (u16d, u16d, 0));
 fu16 (((vector unsigned short){0,0,0,0,0,0,0,0}),
       ((vector unsigned short){0,0,0,0,0,0,0,0}),
       ((vector unsigned short){0,0,0,0,0,0,0,0}),
       ((vector unsigned short){0,0,0,0,0,0,0,0}));
 fu16 (vec_xor(u16a, u16a),
       vec_andc(u16b, u16b),
       vec_sub(u16c, u16c),
       vec_subs(u16d, u16d));
 fu16 (vec_splat_u16(0),
       vec_splat_u16(0),
       vec_splat_u16(0),
       vec_splat_u16(0));
 fu16 (((vector unsigned short){0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff}),
       ((vector unsigned short){0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff}),
       ((vector unsigned short){0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff}),
       ((vector unsigned short){0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff,0xffff}));
 fu16 (vec_splat_u16(-1),
       vec_splat_u16(-1),
       vec_splat_u16(-1),
       vec_splat_u16(-1));
 fu16 ((vector unsigned short)vec_cmpeq(u16a, u16a),
       (vector unsigned short)vec_cmpeq(u16b, u16b),
       (vector unsigned short)vec_cmpeq(u16c, u16c),
       (vector unsigned short)vec_cmpeq(u16d, u16d));
}

extern void fu8(vector unsigned char, vector unsigned char,
		vector unsigned char, vector unsigned char);
void fxu8(vector unsigned char u8a, vector unsigned char u8b,
	   vector unsigned char u8c, vector unsigned char u8d)
{
 fu8 (u8a,
       u8b,
       u8c,
       vec_avg(u8d, u8d));
 fu8 (vec_or (u8a, u8a),
       vec_and (u8b, u8b),
       vec_max (u8c, u8c),
       vec_min (u8d, u8d));
 fu8 (vec_sld (u8a, u8a, 0),
       vec_sld (u8b, u8b, 0),
       vec_sld (u8c, u8c, 0),
       vec_sld (u8d, u8d, 0));
 fu8 (((vector unsigned char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}),
       ((vector unsigned char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}),
       ((vector unsigned char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}),
       ((vector unsigned char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}));
 fu8 (vec_xor(u8a, u8a),
       vec_andc(u8b, u8b),
       vec_sub(u8c, u8c),
       vec_subs(u8d, u8d));
 fu8 (vec_splat_u8(0),
       vec_splat_u8(0),
       vec_splat_u8(0),
       vec_splat_u8(0));
 fu8 (((vector unsigned char){0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff}),
       ((vector unsigned char){0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff}),
       ((vector unsigned char){0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff}),
       ((vector unsigned char){0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff}));
 fu8 (vec_splat_u8(-1),
       vec_splat_u8(-1),
       vec_splat_u8(-1),
       vec_splat_u8(-1));
 fu8 ((vector unsigned char)vec_cmpeq(u8a, u8a),
       (vector unsigned char)vec_cmpeq(u8b, u8b),
       (vector unsigned char)vec_cmpeq(u8c, u8c),
       (vector unsigned char)vec_cmpeq(u8d, u8d));
}

extern void fs32(vector signed int, vector signed int,
		 vector signed int, vector signed int);
void fxs32(vector signed int s32a, vector signed int s32b,
	   vector signed int s32c, vector signed int s32d)
{
 fs32 (s32a,
       s32b,
       s32c,
       vec_avg(s32d, s32d));
 fs32 (vec_or (s32a, s32a),
       vec_and (s32b, s32b),
       vec_max (s32c, s32c),
       vec_min (s32d, s32d));
 fs32 (vec_sld (s32a, s32a, 0),
       vec_sld (s32b, s32b, 0),
       vec_sld (s32c, s32c, 0),
       vec_sld (s32d, s32d, 0));
 fs32 (((vector signed int){0,0,0,0}),
       ((vector signed int){0,0,0,0}),
       ((vector signed int){0,0,0,0}),
       ((vector signed int){0,0,0,0}));
 fs32 (vec_xor(s32a, s32a),
       vec_andc(s32b, s32b),
       vec_sub(s32c, s32c),
       vec_subs(s32d, s32d));
 fs32 (vec_splat_s32(0),
       vec_splat_s32(0),
       vec_splat_s32(0),
       vec_splat_s32(0));
 fs32 (((vector signed int){-1,-1,-1,-1}),
       ((vector signed int){-1,-1,-1,-1}),
       ((vector signed int){-1,-1,-1,-1}),
       ((vector signed int){-1,-1,-1,-1}));
 fs32 (vec_splat_s32(-1),
       vec_splat_s32(-1),
       vec_splat_s32(-1),
       vec_splat_s32(-1));
 fs32 ((vector signed int)vec_cmpeq(s32a, s32a),
       (vector signed int)vec_cmpeq(s32b, s32b),
       (vector signed int)vec_cmpeq(s32c, s32c),
       (vector signed int)vec_cmpeq(s32d, s32d));
}

extern void fs16(vector signed short, vector signed short,
		 vector signed short, vector signed short);
void fxs16(vector signed short s16a, vector signed short s16b,
	   vector signed short s16c, vector signed short s16d)
{
 fs16 (s16a,
       s16b,
       s16c,
       vec_avg(s16d, s16d));
 fs16 (vec_or (s16a, s16a),
       vec_and (s16b, s16b),
       vec_max (s16c, s16c),
       vec_min (s16d, s16d));
 fs16 (vec_sld (s16a, s16a, 0),
       vec_sld (s16b, s16b, 0),
       vec_sld (s16c, s16c, 0),
       vec_sld (s16d, s16d, 0));
 fs16 (((vector signed short){0,0,0,0,0,0,0,0}),
       ((vector signed short){0,0,0,0,0,0,0,0}),
       ((vector signed short){0,0,0,0,0,0,0,0}),
       ((vector signed short){0,0,0,0,0,0,0,0}));
 fs16 (vec_xor(s16a, s16a),
       vec_andc(s16b, s16b),
       vec_sub(s16c, s16c),
       vec_subs(s16d, s16d));
 fs16 (vec_splat_s16(0),
       vec_splat_s16(0),
       vec_splat_s16(0),
       vec_splat_s16(0));
 fs16 (((vector signed short){-1,-1,-1,-1,-1,-1,-1,-1}),
       ((vector signed short){-1,-1,-1,-1,-1,-1,-1,-1}),
       ((vector signed short){-1,-1,-1,-1,-1,-1,-1,-1}),
       ((vector signed short){-1,-1,-1,-1,-1,-1,-1,-1}));
 fs16 (vec_splat_s16(-1),
       vec_splat_s16(-1),
       vec_splat_s16(-1),
       vec_splat_s16(-1));
 fs16 ((vector signed short)vec_cmpeq(s16a, s16a),
       (vector signed short)vec_cmpeq(s16b, s16b),
       (vector signed short)vec_cmpeq(s16c, s16c),
       (vector signed short)vec_cmpeq(s16d, s16d));
}

extern void fs8(vector signed char, vector signed char,
		vector signed char, vector signed char);
void fxs8(vector signed char s8a, vector signed char s8b,
	   vector signed char s8c, vector signed char s8d)
{
 fs8 (s8a,
       s8b,
       s8c,
       vec_avg(s8d, s8d));
 fs8 (vec_or (s8a, s8a),
       vec_and (s8b, s8b),
       vec_max (s8c, s8c),
       vec_min (s8d, s8d));
 fs8 (vec_sld (s8a, s8a, 0),
       vec_sld (s8b, s8b, 0),
       vec_sld (s8c, s8c, 0),
       vec_sld (s8d, s8d, 0));
 fs8 (((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}),
       ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}),
       ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}),
       ((vector signed char){0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}));
 fs8 (vec_xor(s8a, s8a),
       vec_andc(s8b, s8b),
       vec_sub(s8c, s8c),
       vec_subs(s8d, s8d));
 fs8 (vec_splat_s8(0),
       vec_splat_s8(0),
       vec_splat_s8(0),
       vec_splat_s8(0));
 fs8 (((vector signed char){-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}),
       ((vector signed char){-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}),
       ((vector signed char){-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}),
       ((vector signed char){-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}));
 fs8 (vec_splat_s8(-1),
       vec_splat_s8(-1),
       vec_splat_s8(-1),
       vec_splat_s8(-1));
 fs8 ((vector signed char)vec_cmpeq(s8a, s8a),
       (vector signed char)vec_cmpeq(s8b, s8b),
       (vector signed char)vec_cmpeq(s8c, s8c),
       (vector signed char)vec_cmpeq(s8d, s8d));
}

void fu32(vector unsigned int a, vector unsigned int b,
	  vector unsigned int c, vector unsigned int d)
{
}

void fu16(vector unsigned short a, vector unsigned short b,
	  vector unsigned short c, vector unsigned short d)
{
}

void fu8(vector unsigned char a, vector unsigned char b,
	 vector unsigned char c, vector unsigned char d)
{
}

void fs32(vector signed int a, vector signed int b,
	  vector signed int c, vector signed int d)
{
}

void fs16(vector signed short a, vector signed short b,
	  vector signed short c, vector signed short d)
{
}

void fs8(vector signed char a, vector signed char b,
	 vector signed char c, vector signed char d)
{
}
