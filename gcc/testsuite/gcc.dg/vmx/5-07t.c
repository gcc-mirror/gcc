/* { dg-do compile } */
#include <altivec.h>
typedef vector unsigned char t_u8;
typedef vector signed char t_s8;
typedef vector bool char t_b8;
typedef vector unsigned short t_u16;
typedef vector signed short t_s16;
typedef vector bool short t_b16;
typedef vector unsigned int t_u32;
typedef vector signed int t_s32;
typedef vector bool int t_b32;
typedef vector float t_f32;
typedef vector pixel t_p16;

t_u8 u8 = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
t_s8 s8 = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
t_b8 b8 = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
t_u16 u16 = {1,1,1,1,1,1,1,1};
t_s16 s16 = {1,1,1,1,1,1,1,1};
t_b16 b16 = {1,1,1,1,1,1,1,1};
t_u32 u32 = {1,1,1,1};
t_s32 s32 = {1,1,1,1};
t_b32 b32 = {1,1,1,1};
t_f32 f32 = {1,1,1,1};
t_p16 p16 = {1,1,1,1,1,1,1,1};

t_u8 u8_ = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
t_s8 s8_ = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
t_b8 b8_ = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
t_u16 u16_ = {1,2,3,4,5,6,7,8};
t_s16 s16_ = {1,2,3,4,5,6,7,8};
t_b16 b16_ = {1,2,3,4,5,6,7,8};
t_u32 u32_ = {1,2,3,4};
t_s32 s32_ = {1,2,3,4};
t_b32 b32_ = {1,2,3,4};
t_f32 f32_ = {1,2,3,4};
t_p16 p16_ = {1,2,3,4,5,6,7,8};
