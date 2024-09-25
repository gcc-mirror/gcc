/* Test reduc support for MIPS MSA ASE with shf.FMT */
/* { dg-do compile } */
/* { dg-skip-if "auto-vectorization test" { *-*-* } { "-O0" "-O1" "-O2" "-Os"} { "-O3" } } */
/* { dg-options "-mfp64 -mhard-float -mmsa -ffast-math" } */

/* { dg-final { scan-assembler-times "min_int8_t:.*shf\\.b.*0xb1.*min_s\\.b.*shf\\.h.*0xb1.*min_s\\.b.*shf\\.w.*0xb1.*min_s\\.b.*shf\\.w.*0x4e.*min_s\\.b.*copy_s\\.b.*min_int8_t" 1 } } */
/* { dg-final { scan-assembler-times "max_int8_t:.*shf\\.b.*0xb1.*max_s\\.b.*shf\\.h.*0xb1.*max_s\\.b.*shf\\.w.*0xb1.*max_s\\.b.*shf\\.w.*0x4e.*max_s\\.b.*copy_s\\.b.*max_int8_t" 1 } } */
/* { dg-final { scan-assembler-times "min_uint8_t:.*shf\\.b.*0xb1.*min_u\\.b.*shf\\.h.*0xb1.*min_u\\.b.*shf\\.w.*0xb1.*min_u\\.b.*shf\\.w.*0x4e.*min_u\\.b.*copy_.\\.b.*min_uint8_t" 1 } } */
/* { dg-final { scan-assembler-times "max_uint8_t:.*shf\\.b.*0xb1.*max_u\\.b.*shf\\.h.*0xb1.*max_u\\.b.*shf\\.w.*0xb1.*max_u\\.b.*shf\\.w.*0x4e.*max_u\\.b.*copy_.\\.b.*max_uint8_t" 1 } } */
/* { dg-final { scan-assembler-times "min_int16_t:.*shf\\.h.*0xb1.*min_s\\.h.*shf\\.w.*0xb1.*min_s\\.h.*shf\\.w.*0x4e.*min_s\\.h.*copy_s\\.h.*min_int16_t" 1 } } */
/* { dg-final { scan-assembler-times "max_int16_t:.*shf\\.h.*0xb1.*max_s\\.h.*shf\\.w.*0xb1.*max_s\\.h.*shf\\.w.*0x4e.*max_s\\.h.*copy_s\\.h.*max_int16_t" 1 } } */
/* { dg-final { scan-assembler-times "min_uint16_t:.*shf\\.h.*0xb1.*min_u\\.h.*shf\\.w.*0xb1.*min_u\\.h.*shf\\.w.*0x4e.*min_u\\.h.*copy_.\\.h.*min_uint16_t" 1 } } */
/* { dg-final { scan-assembler-times "max_uint16_t:.*shf\\.h.*0xb1.*max_u\\.h.*shf\\.w.*0xb1.*max_u\\.h.*shf\\.w.*0x4e.*max_u\\.h.*copy_.\\.h.*max_uint16_t" 1 } } */
/* { dg-final { scan-assembler-times "min_int32_t:.*shf\\.w.*0xb1.*min_s\\.w.*shf\\.w.*0x4e.*min_s\\.w.*copy_s\\.w.*min_int32_t" 1 } } */
/* { dg-final { scan-assembler-times "max_int32_t:.*shf\\.w.*0xb1.*max_s\\.w.*shf\\.w.*0x4e.*max_s\\.w.*copy_s\\.w.*max_int32_t" 1 } } */
/* { dg-final { scan-assembler-times "min_uint32_t:.*shf\\.w.*0xb1.*min_u\\.w.*shf\\.w.*0x4e.*min_u\\.w.*copy_.\\.w.*min_uint32_t" 1 } } */
/* { dg-final { scan-assembler-times "max_uint32_t:.*shf\\.w.*0xb1.*max_u\\.w.*shf\\.w.*0x4e.*max_u\\.w.*copy_.\\.w.*max_uint32_t" 1 } } */
/* { dg-final { scan-assembler-times "min_int64_t:.*shf\\.w.*0x4e.*min_s\\.d.*copy_s\\.?.*min_int64_t" 1 } } */
/* { dg-final { scan-assembler-times "max_int64_t:.*shf\\.w.*0x4e.*max_s\\.d.*copy_s\\.?.*max_int64_t" 1 } } */
/* { dg-final { scan-assembler-times "min_uint64_t:.*shf\\.w.*0x4e.*min_u\\.d.*copy_.\\.?.*min_uint64_t" 1 } } */
/* { dg-final { scan-assembler-times "max_uint64_t:.*shf\\.w.*0x4e.*max_u\\.d.*copy_.\\.?.*max_uint64_t" 1 } } */
/* { dg-final { scan-assembler-times "min_float:.*shf\\.w.*0xb1.*fmin\\.w.*shf\\.w.*0x4e.*fmin\\.w.*min_float" 1 } } */
/* { dg-final { scan-assembler-times "max_float:.*shf\\.w.*0xb1.*fmax\\.w.*shf\\.w.*0x4e.*fmax\\.w.*max_float" 1 } } */
/* { dg-final { scan-assembler-times "min_double:.*shf\\.w.*0x4e.*fmin\\.d.*min_double" 1 } } */
/* { dg-final { scan-assembler-times "max_double:.*shf\\.w.*0x4e.*fmax\\.d.*max_double" 1 } } */

/* { dg-final { scan-assembler-times "plus_int8_t:.*hadd_s\\.h.*hadd_s\\.w.*hadd_s\\.d.*shf\\.w.*0x4e.*addv\\.d.*copy_s\\.b.*plus_int8_t" 1 } } */
/* { dg-final { scan-assembler-times "plus_uint8_t:.*hadd_s\\.h.*hadd_s\\.w.*hadd_s\\.d.*shf\\.w.*0x4e.*addv\\.d.*copy_.\\.b.*plus_uint8_t" 1 } } */
/* { dg-final { scan-assembler-times "plus_int16_t:.*hadd_s\\.w.*hadd_s\\.d.*shf\\.w.*0x4e.*addv\\.d.*copy_s\\.h.*plus_int16_t" 1 } } */
/* { dg-final { scan-assembler-times "plus_uint16_t:.*hadd_s\\.w.*hadd_s\\.d.*shf\\.w.*0x4e.*addv\\.d.*copy_.\\.h.*plus_uint16_t" 1 } } */
/* { dg-final { scan-assembler-times "plus_int32_t:.*hadd_s\\.d.*copy_s\\.w.*copy_s\\.w.*addu.*plus_int32_t" 1 } } */
/* { dg-final { scan-assembler-times "plus_uint32_t:.*hadd_s\\.d.*copy_s\\.w.*copy_s\\.w.*addu.*plus_uint32_t" 1 } } */
/* { dg-final { scan-assembler-times "plus_int64_t:.*shf\\.w.*0x4e.*addv\\.d.*copy_s\\.?.*plus_int64_t" 1 } } */
/* { dg-final { scan-assembler-times "plus_uint64_t:.*shf\\.w.*0x4e.*addv\\.d.*copy_.\\.?.*plus_uint64_t" 1 } } */
/* { dg-final { scan-assembler-times "plus_float:.*shf\\.w.*0xb1.*fadd\\.w.*shf\\.w.*0x4e.*fadd\\.w.*plus_float" 1 } } */
/* { dg-final { scan-assembler-times "plus_double:.*shf\\.w.*0x4e.*fadd\\.d.*plus_double" 1 } } */

/* { dg-final { scan-assembler-times "or_int8_t:.*shf\\.b.*0xb1.*or\\.v.*shf\\.h.*0xb1.*or\\.v.*shf\\.w.*0xb1.*or\\.v.*shf\\.w.*0x4e.*or\\.v.*copy_s\\.b.*or_int8_t" 1 } } */
/* { dg-final { scan-assembler-times "xor_int8_t:.*shf\\.b.*0xb1.*xor\\.v.*shf\\.h.*0xb1.*xor\\.v.*shf\\.w.*0xb1.*xor\\.v.*shf\\.w.*0x4e.*xor\\.v.*copy_s\\.b.*xor_int8_t" 1 } } */
/* { dg-final { scan-assembler-times "and_int8_t:.*shf\\.b.*0xb1.*and\\.v.*shf\\.h.*0xb1.*and\\.v.*shf\\.w.*0xb1.*and\\.v.*shf\\.w.*0x4e.*and\\.v.*copy_s\\.b.*and_int8_t" 1 } } */
/* { dg-final { scan-assembler-times "or_uint8_t:.*shf\\.b.*0xb1.*or\\.v.*shf\\.h.*0xb1.*or\\.v.*shf\\.w.*0xb1.*or\\.v.*shf\\.w.*0x4e.*or\\.v.*copy_.\\.b.*or_uint8_t" 1 } } */
/* { dg-final { scan-assembler-times "xor_uint8_t:.*shf\\.b.*0xb1.*xor\\.v.*shf\\.h.*0xb1.*xor\\.v.*shf\\.w.*0xb1.*xor\\.v.*shf\\.w.*0x4e.*xor\\.v.*copy_.\\.b.*xor_uint8_t" 1 } } */
/* { dg-final { scan-assembler-times "and_uint8_t:.*shf\\.b.*0xb1.*and\\.v.*shf\\.h.*0xb1.*and\\.v.*shf\\.w.*0xb1.*and\\.v.*shf\\.w.*0x4e.*and\\.v.*copy_.\\.b.*and_uint8_t" 1 } } */
/* { dg-final { scan-assembler-times "or_int16_t:.*shf\\.h.*0xb1.*or\\.v.*shf\\.w.*0xb1.*or\\.v.*shf\\.w.*0x4e.*or\\.v.*copy_s\\.h.*or_int16_t" 1 } } */
/* { dg-final { scan-assembler-times "xor_int16_t:.*shf\\.h.*0xb1.*xor\\.v.*shf\\.w.*0xb1.*xor\\.v.*shf\\.w.*0x4e.*xor\\.v.*copy_s\\.h.*xor_int16_t" 1 } } */
/* { dg-final { scan-assembler-times "and_int16_t:.*shf\\.h.*0xb1.*and\\.v.*shf\\.w.*0xb1.*and\\.v.*shf\\.w.*0x4e.*and\\.v.*copy_s\\.h.*and_int16_t" 1 } } */
/* { dg-final { scan-assembler-times "or_uint16_t:.*shf\\.h.*0xb1.*or\\.v.*shf\\.w.*0xb1.*or\\.v.*shf\\.w.*0x4e.*or\\.v.*copy_.\\.h.*or_uint16_t" 1 } } */
/* { dg-final { scan-assembler-times "xor_uint16_t:.*shf\\.h.*0xb1.*xor\\.v.*shf\\.w.*0xb1.*xor\\.v.*shf\\.w.*0x4e.*xor\\.v.*copy_.\\.h.*xor_uint16_t" 1 } } */
/* { dg-final { scan-assembler-times "and_uint16_t:.*shf\\.h.*0xb1.*and\\.v.*shf\\.w.*0xb1.*and\\.v.*shf\\.w.*0x4e.*and\\.v.*copy_.\\.h.*and_uint16_t" 1 } } */
/* { dg-final { scan-assembler-times "or_int32_t:.*shf\\.w.*0xb1.*or\\.v.*shf\\.w.*0x4e.*or\\.v.*copy_s\\.w.*or_int32_t" 1 } } */
/* { dg-final { scan-assembler-times "xor_int32_t:.*shf\\.w.*0xb1.*xor\\.v.*shf\\.w.*0x4e.*xor\\.v.*copy_s\\.w.*xor_int32_t" 1 } } */
/* { dg-final { scan-assembler-times "and_int32_t:.*shf\\.w.*0xb1.*and\\.v.*shf\\.w.*0x4e.*and\\.v.*copy_s\\.w.*and_int32_t" 1 } } */
/* { dg-final { scan-assembler-times "or_uint32_t:.*shf\\.w.*0xb1.*or\\.v.*shf\\.w.*0x4e.*or\\.v.*copy_.\\.w.*or_uint32_t" 1 } } */
/* { dg-final { scan-assembler-times "xor_uint32_t:.*shf\\.w.*0xb1.*xor\\.v.*shf\\.w.*0x4e.*xor\\.v.*copy_.\\.w.*xor_uint32_t" 1 } } */
/* { dg-final { scan-assembler-times "and_uint32_t:.*shf\\.w.*0xb1.*and\\.v.*shf\\.w.*0x4e.*and\\.v.*copy_.\\.w.*and_uint32_t" 1 } } */
/* { dg-final { scan-assembler-times "or_int64_t:.*shf\\.w.*0x4e.*or\\.v.*copy_s\\.?.*or_int64_t" 1 } } */
/* { dg-final { scan-assembler-times "xor_int64_t:.*shf\\.w.*0x4e.*xor\\.v.*copy_s\\.?.*xor_int64_t" 1 } } */
/* { dg-final { scan-assembler-times "and_int64_t:.*shf\\.w.*0x4e.*and\\.v.*copy_s\\.?.*and_int64_t" 1 } } */
/* { dg-final { scan-assembler-times "or_uint64_t:.*shf\\.w.*0x4e.*or\\.v.*copy_.\\.?.*or_uint64_t" 1 } } */
/* { dg-final { scan-assembler-times "xor_uint64_t:.*shf\\.w.*0x4e.*xor\\.v.*copy_.\\.?.*xor_uint64_t" 1 } } */
/* { dg-final { scan-assembler-times "and_uint64_t:.*shf\\.w.*0x4e.*and\\.v.*copy_.\\.?.*and_uint64_t" 1 } } */

#include <stdint.h>

#define D_TY_CALC(type) \
	type a_##type[32] __attribute__ ((aligned (16))); \
	type min_##type () { \
		type ret = a_##type[0];	\
		for (int i=0; i<32; i++) \
			ret = (ret < a_##type[i]) ? ret : a_##type[i]; \
		return ret;	\
	}	\
	type max_##type () { \
		type ret = a_##type[0];	\
		for (int i=0; i<32; i++) \
			ret = (ret > a_##type[i]) ? ret : a_##type[i]; \
		return ret;	\
	}	\
	type plus_##type () { \
		type ret = 0;	\
		for (int i=0; i<32; i++) \
			ret += a_##type[i]; \
		return ret;	\
	}

#define D_TY_BIT(type)	\
	type or_##type () {	\
		type ret = 0;	\
		for (int i=0; i<32; i++) \
			ret |= a_##type[i]; \
		return ret;	\
	}	\
	type and_##type () {	\
		type ret = (type)(long long)~0LL;	\
		for (int i=0; i<32; i++) \
			ret &= a_##type[i]; \
		return ret;	\
	}	\
	type xor_##type () {	\
		type ret = (type)(long long)~0LL;	\
		for (int i=0; i<32; i++) \
			ret ^= a_##type[i]; \
		return ret;	\
	}

#define D_TY(type) D_TY_CALC(type) D_TY_BIT(type)

D_TY (int8_t)
D_TY (uint8_t)
D_TY (int16_t)
D_TY (uint16_t)
D_TY (int32_t)
D_TY (uint32_t)
D_TY (int64_t)
D_TY (uint64_t)
D_TY_CALC (float)
D_TY_CALC (double)


