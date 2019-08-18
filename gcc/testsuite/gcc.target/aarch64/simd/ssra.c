/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O3" } */
/* { dg-skip-if "" { *-*-* } {"*sve*"} {""} } */

#include <stdint.h>

#define SSRA(func, vtype, n)				\
	void func ()					\
	{						\
	    int i;					\
	    for (i = 0; i < n; i++)			\
	    {						\
		s1##vtype[i] += s2##vtype[i] >> 2;	\
	    }						\
	}

#define TEST_VDQ_I_MODES(FUNC)				\
	FUNC (test_v8qi_v16qi, _char, 16)		\
	FUNC (test_v4hi_v8h1, _short, 8)		\
	FUNC (test_v2si_v4si, _int, 4)			\
	FUNC (test_v2di, _ll, 2)			\

int8_t s1_char[16], s2_char[16];
int16_t s1_short[8], s2_short[8];
int32_t s1_int[4], s2_int[4];
int64_t s1_ll[2], s2_ll[2];

TEST_VDQ_I_MODES(SSRA)

/* { dg-final { scan-assembler "ssra" } } */
/* { dg-final { scan-assembler-not "sshr" } } */

/* { dg-final { scan-assembler-times {ssra\tv[0-9]+\.16b, v[0-9]+\.16b, [0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {ssra\tv[0-9]+\.8h, v[0-9]+\.8h, [0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {ssra\tv[0-9]+\.4s, v[0-9]+\.4s, [0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {ssra\tv[0-9]+\.2d, v[0-9]+\.2d, [0-9]+} 1 } } */
