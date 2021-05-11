/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z13_hw } } } */

/*
 * The vector intrinsic vec_permi(a, b, c) chooses one of the two eight-byte
 * vector elements in each of a and b, depending on the value of c. The valid
 * values for c differ from the encoding for the M4 field in assembly and in the
 * binary instruction.
 *
 * selection | c | encoding in assembly
 * a[0] b[0] | 0 | 0
 * a[0] b[1] | 1 | 1
 * a[1] b[0] | 2 | 4
 * a[1] b[1] | 3 | 5
 *
 * (i.e., indices a[i] b[j] are encoded for c as (i<<1) | j, yet for the
 * M4 field as (i<<2) | j.
 */
#include <assert.h>
#include <vecintrin.h>

typedef unsigned long long uv2di __attribute__((vector_size(16)));

__attribute__ ((noipa)) static uv2di
do_vec_permi(uv2di a, uv2di b, int c)
{
    switch(c) {
	case 0: return vec_permi(a, b, 0);
	case 1: return vec_permi(a, b, 1);
	case 2: return vec_permi(a, b, 2);
	case 3: return vec_permi(a, b, 3);
	default: assert(0);
    }
}

/* { dg-final { scan-assembler-times {\n\tvpdi\t%v\d+,%v\d+,%v\d+,0\n} 1 } } */
/* { dg-final { scan-assembler-times {\n\tvpdi\t%v\d+,%v\d+,%v\d+,1\n} 1 } } */
/* { dg-final { scan-assembler-times {\n\tvpdi\t%v\d+,%v\d+,%v\d+,4\n} 1 } } */
/* { dg-final { scan-assembler-times {\n\tvpdi\t%v\d+,%v\d+,%v\d+,5\n} 1 } } */

int
main (void)
{
    uv2di a = { 0xa0, 0xa1 };
    uv2di b = { 0xb0, 0xb1 };

    for (int i = 0; i < 2; i++)
	for (int j = 0; j < 2; j++) {
	    uv2di res = do_vec_permi(a, b, (i<<1)|j);
	    assert(res[0] == a[i]);
	    assert(res[1] == b[j]);
	}
}
