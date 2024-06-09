/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

static size_t
utf8_count_rvv(char const *buf, size_t len)
{
	size_t sum = 0;
	for (size_t vl; len > 0; len -= vl, buf += vl) {
		vl = __riscv_vsetvl_e8m8(len);
		vint8m8_t v = __riscv_vle8_v_i8m8((void*)buf, vl);
		vbool1_t mask = __riscv_vmsgt_vx_i8m8_b1(v, -65, vl);
		sum += __riscv_vcpop_m_b1(mask, vl);
	}
	return sum;
}

size_t
utf8_count_rvv_4x_tail(char const *buf, size_t len)
{
	size_t sum = 0;
	size_t vl = __riscv_vsetvlmax_e8m8();
	size_t step = vl * 4;
	const char *it = buf, *end = buf + len;
	for(; it + step <= end; ) {
		vint8m8_t v0 = __riscv_vle8_v_i8m8((void*)it, vl); it += vl;
		vint8m8_t v1 = __riscv_vle8_v_i8m8((void*)it, vl); it += vl;
		vint8m8_t v2 = __riscv_vle8_v_i8m8((void*)it, vl); it += vl;
		vint8m8_t v3 = __riscv_vle8_v_i8m8((void*)it, vl); it += vl;
		vbool1_t m0 = __riscv_vmsgt_vx_i8m8_b1(v0, -65, vl);
		vbool1_t m1 = __riscv_vmsgt_vx_i8m8_b1(v1, -65, vl);
		vbool1_t m2 = __riscv_vmsgt_vx_i8m8_b1(v2, -65, vl);
		vbool1_t m3 = __riscv_vmsgt_vx_i8m8_b1(v3, -65, vl);
		sum += __riscv_vcpop_m_b1(m0, vl);
		sum += __riscv_vcpop_m_b1(m1, vl);
		sum += __riscv_vcpop_m_b1(m2, vl);
		sum += __riscv_vcpop_m_b1(m3, vl);
	}
	return sum + utf8_count_rvv(it, end - it);
}

/* { dg-final { scan-assembler-times {vsetvli} 2 } } */
/* { dg-final { scan-assembler-not {vsetivli} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*m8,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]} 1 } } */
