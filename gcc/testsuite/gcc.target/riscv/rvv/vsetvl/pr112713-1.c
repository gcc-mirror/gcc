/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

size_t
foo (char const *buf, size_t len)
{
	size_t sum = 0;
	size_t vl = __riscv_vsetvlmax_e8m8();
	size_t step = vl * 4;
	const char *it = buf, *end = buf + len;
	for(; it + step <= end; ) {
		it += vl;
		vint8m8_t v3 = __riscv_vle8_v_i8m8((void*)it, vl); it += vl;
		vbool1_t m3 = __riscv_vmsgt_vx_i8m8_b1(v3, -65, vl);
		sum += __riscv_vcpop_m_b1(m3, vl);
	}
	return sum;
}

/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
/* { dg-final { scan-assembler-not {vsetivli} } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*t[au],\s*m[au]} 1 } } */
