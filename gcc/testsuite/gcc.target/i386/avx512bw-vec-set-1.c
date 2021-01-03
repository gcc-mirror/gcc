/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512bw -O2" } */
/* { dg-final { scan-assembler-times {(?n)(?:vp?broadcast|vmovddup)} 36 } } */
/* { dg-final { scan-assembler-times {(?n)vpcmp[bwdq][ \t]+\$0} 18 } } */

typedef char v64qi __attribute__ ((vector_size (64)));
typedef short v32hi __attribute__ ((vector_size (64)));
typedef int v16si __attribute__ ((vector_size (64)));
typedef long long v8di __attribute__ ((vector_size (64)));
typedef float v16sf __attribute__ ((vector_size (64)));
typedef double v8df __attribute__ ((vector_size (64)));

#include "avx2-vec-set-1.c"

FOO (v64qi, char);
FOO (v32hi, short);
FOO (v16si, int);
FOO (v8di, long long);
FOO (v16sf, float);
FOO (v8df, double);
