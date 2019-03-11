/* { dg-do run { target { powerpc*-*-* && cell_hw } } } */
/* { dg-do compile { target { powerpc*-*-* && { ! cell_hw } } } } */
/* { dg-require-effective-target powerpc_ppu_ok } */
/* { dg-options "-O2 -maltivec -mabi=altivec -mdejagnu-cpu=cell" } */
#include <altivec.h>
#include <string.h>

extern void abort (void);

typedef short int sint16;
typedef signed char int8;

int main1(void) __attribute__((noinline));
int main1(void)
{
    sint16 test_vector[4] = { 1678, -2356, 19246, -17892 };
    int8 test_dst[128] __attribute__(( aligned( 16 )));
    float test_out[4] __attribute__(( aligned( 16 )));
    int p;

    for( p = 0; p < 24; ++p )
    {
        memset( test_dst, 0, 128 );
        memcpy( &test_dst[p], test_vector, 8 );
        {
            vector float VR, VL, V;
   /* load the righthand section of the misaligned vector */
            VR = (vector float) vec_lvrx( 8, &test_dst[p] );
            VL = (vector float) vec_lvlx( 0, &test_dst[p] );
   /* Vector Shift Left Double by Octet Immediate, move the right hand section into the bytes */
            VR = vec_vsldoi( VR, VR, 2 << 2 ); 
   /* or those two together */
            V = vec_vor( VL, VR );
   /* sign extend */
            V = (vector float) vec_vupkhsh((vector bool short)V );
   /* fixed to float by S16_SHIFT_BITS bits */
            V = (vector float) vec_vcfsx ((vector signed int)V, 5 ); 

            vec_stvx( V, 0, &test_out[0] );
            if (test_out[0] != 52.437500)
                abort ();
            if (test_out[1] != -73.625000)
                abort ();
            if (test_out[2] != 601.437500)
                abort ();
            if (test_out[3] != -559.125000)
                abort ();
        }
    }
return 0;
}


int main(void)
{
  return main1();
}
