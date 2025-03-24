/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -pedantic-errors -march=k8 -msse4a -m3dnow -mavx -mavx2 -mfma4 -mxop -maes -mpclmul -mpopcnt -mabm -mlzcnt -mbmi -mbmi2 -mtbm -mlwp -mfsgsbase -mrdrnd -mf16c -mfma -mrtm -mrdseed -mprfchw -madx -mfxsr -mxsaveopt -msha -mxsavec -mxsaves -mclflushopt -mclwb -mmwaitx -mclzero -mpku -msgx -mrdpid -mgfni -mpconfig -mwbnoinvd -menqcmd -mavx512vp2intersect -mserialize -mtsxldtrk -mamx-tile -mamx-int8 -mamx-bf16 -mkl -mwidekl -mavxvnni -mavxifma -mavxvnniint8 -mavxneconvert -mcmpccxadd -mamx-fp16 -mprefetchi -mraoint -mamx-complex -mavxvnniint16 -msm3 -msha512 -msm4 -mavx10.2 -mamx-avx512 -mamx-tf32 -mamx-transpose -mamx-fp8 -mmovrs -mamx-movrs" } */
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

/* Test that {,x,e,p,t,s,w,a,b,i}mmintrin.h, mm3dnow.h, fma4intrin.h,
   xopintrin.h, abmintrin.h, bmiintrin.h, tbmintrin.h, lwpintrin.h,
   popcntintrin.h, fmaintrin.h, pkuintrin.h, avx512vpopcntdqintrin.h,
   gfniintrin.h, avx512bitalgintrin.h, avx512vp2intersectintrin.h,
   tsxldtrkintrin.h, amxtileintrin.h, amxint8intrin.h, amxbf16intrin.h,
   avx512vp2intersectvlintrin.h and mm_malloc.h.h are usable
   with -O -pedantic-errors.  */

#include <x86intrin.h>

int dummy;

