/* { dg-do compile } */
#include <altivec.h>
vector  unsigned char   u8a,  u8b,  u8c,  u8d, *u8ptr;
vector  signed short s16a, s16b, s16c, s16d;
vector  unsigned short u16a, u16b, u16c, u16d;
vector  unsigned int u32a, u32b, u32c, u32d;
vector  float f32a, f32b, f32c, f32d, f32e;
int i, j, *p;

void test()
{
        u8c  = vec_add(u8a, u8b);
        f32c = vec_ceil(f32a);
        f32d = vec_vcfux(u32a, 31U);
        s16c = vec_splat_s16(-16);
        u8d  = vec_vsldoi(u8a, u8b, 15);
        f32e = vec_vmaddfp(f32a, f32b, f32c);
         
        vec_dss(3);
        vec_dssall();
        vec_mtvscr(u8a);
        u16a = vec_mfvscr();
}
