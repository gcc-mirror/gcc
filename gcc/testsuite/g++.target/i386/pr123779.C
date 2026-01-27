/* PR target/123779 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mavx512bw" } */

typedef char __v16qi __attribute__((__vector_size__(16)));
typedef short __v8hi __attribute__((__vector_size__(16)));
typedef int __v4si __attribute__((__vector_size__(16)));
typedef long long __v2di __attribute__((__vector_size__(16)));

typedef int __v8si __attribute__((__vector_size__(32)));
typedef long long __v4di __attribute__((__vector_size__(32)));
typedef long long __v8di __attribute__((__vector_size__(64)));
typedef unsigned char __mmask8;

long long g_mem;

#define MAKE_TEST_BLEND(NAME, DEST_T, SRC_INIT_T, PMOV_INPUT_CAST_T, \
			PMOV_FUNC, BLEND_FUNC, BLEND_CAST_T, LOAD_VAL_TYPE, ...) \
    DEST_T dest_##NAME, mask_src1_##NAME, mask_src2_##NAME, mask_##NAME, res_##NAME; \
    void test_##NAME() \
      { \
        mask_##NAME = mask_src1_##NAME < mask_src2_##NAME; \
        LOAD_VAL_TYPE val = (LOAD_VAL_TYPE)g_mem; \
        SRC_INIT_T src_vec = { __VA_ARGS__ }; \
        DEST_T extended = (DEST_T)PMOV_FUNC ((PMOV_INPUT_CAST_T)src_vec); \
        res_##NAME = (DEST_T)BLEND_FUNC ( \
            (BLEND_CAST_T)dest_##NAME, \
            (BLEND_CAST_T)extended, \
            (BLEND_CAST_T)mask_##NAME); \
      }

#define MAKE_TEST_MASK(NAME, DEST_T, SRC_INIT_T, PMOV_INPUT_CAST_T, \
		       PMOV_MASK_FUNC, LOAD_VAL_TYPE, ...) \
    DEST_T dest_##NAME, res_##NAME; \
    __mmask8 mask_##NAME; \
    void test_##NAME() \
      { \
	LOAD_VAL_TYPE val = (LOAD_VAL_TYPE) g_mem; \
	SRC_INIT_T src_vec = { __VA_ARGS__ }; \
	res_##NAME = PMOV_MASK_FUNC ((PMOV_INPUT_CAST_T)src_vec, \
				     dest_##NAME, \
				     mask_##NAME);\
      }  


MAKE_TEST_BLEND(v8qi_v8hi, __v8hi, __v2di, __v16qi, __builtin_ia32_pmovzxbw128, __builtin_ia32_pblendvb128, __v16qi, long long, val, 0)
MAKE_TEST_BLEND(v4qi_v4si, __v4si, __v4si, __v16qi, __builtin_ia32_pmovzxbd128, __builtin_ia32_pblendvb128, __v16qi, int, val, 0, 0, 0)
MAKE_TEST_BLEND(v4hi_v4si, __v4si, __v2di, __v8hi, __builtin_ia32_pmovzxwd128, __builtin_ia32_pblendvb128, __v16qi, long long, val, 0)
MAKE_TEST_BLEND(v2hi_v2di, __v2di, __v4si, __v8hi, __builtin_ia32_pmovzxwq128, __builtin_ia32_pblendvb128, __v16qi, int, val, 0, 0, 0)
MAKE_TEST_BLEND(v2si_v2di, __v2di, __v2di, __v4si, __builtin_ia32_pmovzxdq128, __builtin_ia32_pblendvb128, __v16qi, long long, val, 0)


MAKE_TEST_MASK(v8qi_v8si, __v8si, __v2di, __v16qi, __builtin_ia32_pmovzxbd256_mask, long long, val, 0)
MAKE_TEST_MASK(v4qi_v4di, __v4di, __v4si, __v16qi, __builtin_ia32_pmovzxbq256_mask, int, val, 0, 0, 0)
MAKE_TEST_MASK(v4hi_v4di, __v4di, __v2di, __v8hi, __builtin_ia32_pmovzxwq256_mask, long long, val, 0)
MAKE_TEST_MASK(v8qi_v8di, __v8di, __v2di, __v16qi, __builtin_ia32_pmovzxbq512_mask, long long, val, 0)
