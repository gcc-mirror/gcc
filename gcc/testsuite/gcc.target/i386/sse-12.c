/* Test that {,x,e,p,t,s,w,a,b,i}mmintrin.h, mm3dnow.h, fma4intrin.h,
   xopintrin.h, abmintrin.h, bmiintrin.h, tbmintrin.h, lwpintrin.h,
   popcntintrin.h gfniintrin.h and mm_malloc.h are usable
   with -O -std=c89 -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-O -std=c89 -pedantic-errors -march=k8 -msse4a -m3dnow -mavx -mavx2 -mfma4 -mxop -maes -mpclmul -mpopcnt -mabm -mlzcnt -mbmi -mbmi2 -mtbm -mlwp -mfsgsbase -mrdrnd -mf16c -mfma -mrtm -mrdseed -mprfchw -madx -mfxsr -mxsaveopt -msha -mxsavec -mxsaves -mclflushopt -mclwb -mmwaitx -mclzero -mpku -msgx -mrdpid -mgfni -mpconfig -mwbnoinvd -menqcmd -mavx512vp2intersect -mserialize -mtsxldtrk -mamx-tile -mamx-int8 -mamx-bf16 -mkl -mwidekl -mavxvnni -mavxifma -mavxvnniint8 -mavxneconvert -mamx-fp16 -mraoint -mamx-complex -mavxvnniint16 -msm3 -msha512 -msm4 -mavx10.2 -mamx-avx512 -mamx-tf32 -mamx-transpose -mamx-fp8 -mmovrs -mamx-movrs" } */

#include <x86intrin.h>

int dummy;
