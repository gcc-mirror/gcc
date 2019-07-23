/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O3" } */
/* { dg-skip-if "" { *-*-* } {"*sve*"} {""} } */

#include <stdint.h>

#define USRA(func, vtype, n)				\
	void func ()					\
	{						\
	    int i;					\
	    for (i = 0; i < n; i++)			\
	    {						\
		u1##vtype[i] += u2##vtype[i] >> 2;	\
	    }						\
	}

#define TEST_VDQ_I_MODES(FUNC)				\
	FUNC (test_v8qi_v16qi, _char, 16)		\
	FUNC (test_v4hi_v8h1, _short, 8)		\
	FUNC (test_v2si_v4si, _int, 4)			\
	FUNC (test_v2di, _ll, 2)			\

uint8_t u1_char[16], u2_char[16];
uint16_t u1_short[8], u2_short[8];
uint32_t u1_int[4], u2_int[4];
uint64_t u1_ll[2], u2_ll[2];

TEST_VDQ_I_MODES(USRA)

/* { dg-final { scan-assembler "usra" } } */
/* { dg-final { scan-assembler-not "ushr" } } */

/* { dg-final { scan-assembler-times {usra\tv[0-9]+\.16b, v[0-9]+\.16b, [0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {usra\tv[0-9]+\.8h, v[0-9]+\.8h, [0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {usra\tv[0-9]+\.4s, v[0-9]+\.4s, [0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {usra\tv[0-9]+\.2d, v[0-9]+\.2d, [0-9]+} 1 } } */
