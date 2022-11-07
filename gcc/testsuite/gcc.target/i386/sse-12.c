/* Test that {,x,e,p,t,s,w,a,b,i}mmintrin.h, mm3dnow.h, fma4intrin.h,
   xopintrin.h, abmintrin.h, bmiintrin.h, tbmintrin.h, lwpintrin.h,
   popcntintrin.h gfniintrin.h and mm_malloc.h are usable
   with -O -std=c89 -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-O -std=c89 -pedantic-errors -march=k8 -msse4a -m3dnow -mavx -mavx2 -mfma4 -mxop -maes -mpclmul -mpopcnt -mabm -mlzcnt -mbmi -mbmi2 -mtbm -mlwp -mfsgsbase -mrdrnd -mf16c -mfma -mrtm -mrdseed -mprfchw -madx -mfxsr -mxsaveopt -mavx512f -mavx512er -mavx512cd -mavx512pf -msha -mprefetchwt1 -mxsavec -mxsaves -mclflushopt -mavx512bw -mavx512dq -mavx512vl -mavx512vbmi -mavx512vbmi2 -mavx512ifma -mavx5124fmaps -mavx5124vnniw -mavx512vpopcntdq -mclwb -mmwaitx -mclzero -mpku -msgx -mrdpid -mgfni -mavx512bitalg -mpconfig -mwbnoinvd -mavx512bf16 -menqcmd -mavx512vp2intersect -mserialize -mtsxldtrk -mamx-tile -mamx-int8 -mamx-bf16 -mkl -mwidekl -mavxvnni -mavxifma -mavxvnniint8 -mavxneconvert -mamx-fp16 -mraoint" } */

#include <x86intrin.h>

int dummy;
