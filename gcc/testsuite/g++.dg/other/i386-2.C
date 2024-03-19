/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -pedantic-errors -march=k8 -msse4a -m3dnow -mavx -mavx2 -mfma4 -mxop -maes -mpclmul -mpopcnt -mabm -mlzcnt -mbmi -mbmi2 -mtbm -mlwp -mfsgsbase -mrdrnd -mf16c -mfma -mrtm -mrdseed -mprfchw -madx -mfxsr -mxsaveopt -mavx512f -mavx512er -mavx512cd -mavx512pf -msha -mprefetchwt1 -mxsavec -mxsaves -mclflushopt  -mavx512dq -mavx512bw -mavx512vl -mavx512ifma -mavx512vbmi -mavx512vbmi2 -mavx5124fmaps -mavx5124vnniw -mavx512vpopcntdq -mclwb -mmwaitx -mclzero -mpku -msgx -mrdpid -mgfni -mavx512bitalg -mpconfig -mwbnoinvd -mavx512bf16 -menqcmd -mavx512vp2intersect -mserialize -mtsxldtrk -mamx-tile -mamx-int8 -mamx-bf16 -mkl -mwidekl -mavxvnni -mavx512fp16 -mavxifma -mavxvnniint8 -mavxneconvert -mcmpccxadd -mamx-fp16 -mprefetchi -mraoint -mamx-complex -mavxvnniint16 -msm3 -msha512 -msm4" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */
/* { dg-warning "AVX512PF support will be removed in GCC 15" "" { target *-*-* } 0 } */
/* { dg-warning "AVX5124FMAPS support will be removed in GCC 15" "" { target *-*-* } 0 } */
/* { dg-warning "AVX5124VNNIW support will be removed in GCC 15" "" { target *-*-* } 0 } */
/* { dg-warning "PREFETCHWT1 support will be removed in GCC 15" "" { target *-*-* } 0 } */

/* Test that {,x,e,p,t,s,w,a,b,i}mmintrin.h, mm3dnow.h, fma4intrin.h,
   xopintrin.h, abmintrin.h, bmiintrin.h, tbmintrin.h, lwpintrin.h,
   popcntintrin.h, fmaintrin.h, pkuintrin.h, avx5124fmapsintrin.h,
   avx5124vnniwintrin.h, avx512vpopcntdqintrin.h gfniintrin.h
   avx512bitalgintrin.h, avx512vp2intersectintrin.h, tsxldtrkintrin.h,
   amxtileintrin.h, amxint8intrin.h, amxbf16intrin.h,
   avx512vp2intersectvlintrin.h and mm_malloc.h.h are usable
   with -O -pedantic-errors.  */

#include <x86intrin.h>

int dummy;

