#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* In this case, output variables are arrays of vectors.  */
#define DECL_VSHUFFLE(T1, W, N)						\
  VECT_ARRAY_TYPE(T1, W, N, 2) VECT_ARRAY_VAR(result_vec, T1, W, N, 2);	\
  VECT_VAR_DECL(result_bis, T1, W, N)[2 * N]

  /* We need to use a temporary result buffer (result_bis), because
     the one used for other tests is not large enough. A subset of the
     result data is moved from result_bis to result, and it is this
     subset which is used to check the actual behavior. The next
     macro enables to move another chunk of data from result_bis to
     result.  */
#define TEST_VSHUFFLE(INSN, Q, T1, T2, W, N)				\
  VECT_ARRAY_VAR(result_vec, T1, W, N, 2) =				\
    INSN##Q##_##T2##W(VECT_VAR(vector1, T1, W, N),			\
		      VECT_VAR(vector2, T1, W, N));			\
  vst2##Q##_##T2##W(VECT_VAR(result_bis, T1, W, N),			\
		    VECT_ARRAY_VAR(result_vec, T1, W, N, 2));		\
  memcpy(VECT_VAR(result, T1, W, N), VECT_VAR(result_bis, T1, W, N),	\
	 sizeof(VECT_VAR(result, T1, W, N)));

  /* Overwrite "result" with the contents of "result_bis"[X].  */
#define TEST_EXTRA_CHUNK(T1, W, N, X)					\
  memcpy(VECT_VAR(result, T1, W, N), &(VECT_VAR(result_bis, T1, W, N)[X*N]), \
	 sizeof(VECT_VAR(result, T1, W, N)));

  DECL_VARIABLE_ALL_VARIANTS(vector1);
  DECL_VARIABLE_ALL_VARIANTS(vector2);

  /* We don't need 64 bits variants.  */
#define DECL_ALL_VSHUFFLE()				\
  DECL_VSHUFFLE(int, 8, 8);				\
  DECL_VSHUFFLE(int, 16, 4);				\
  DECL_VSHUFFLE(int, 32, 2);				\
  DECL_VSHUFFLE(uint, 8, 8);				\
  DECL_VSHUFFLE(uint, 16, 4);				\
  DECL_VSHUFFLE(uint, 32, 2);				\
  DECL_VSHUFFLE(poly, 8, 8);				\
  DECL_VSHUFFLE(poly, 16, 4);				\
  MFLOAT8_ONLY(DECL_VSHUFFLE(mfloat, 8, 8));		\
  DECL_VSHUFFLE(float, 32, 2);				\
  DECL_VSHUFFLE(int, 8, 16);				\
  DECL_VSHUFFLE(int, 16, 8);				\
  DECL_VSHUFFLE(int, 32, 4);				\
  DECL_VSHUFFLE(uint, 8, 16);				\
  DECL_VSHUFFLE(uint, 16, 8);				\
  DECL_VSHUFFLE(uint, 32, 4);				\
  DECL_VSHUFFLE(poly, 8, 16);				\
  DECL_VSHUFFLE(poly, 16, 8);				\
  MFLOAT8_ONLY(DECL_VSHUFFLE(mfloat, 8, 16));		\
  DECL_VSHUFFLE(float, 32, 4)

  DECL_ALL_VSHUFFLE();
#if defined (FP16_SUPPORTED)
  DECL_VSHUFFLE (float, 16, 4);
  DECL_VSHUFFLE (float, 16, 8);
#endif

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector1, buffer);
#if MFLOAT8_SUPPORTED
  VLOAD (vector1, buffer, , mfloat, mf, 8, 8);
  VLOAD (vector1, buffer, q, mfloat, mf, 8, 16);
#endif
#if defined (FP16_SUPPORTED)
  VLOAD (vector1, buffer, , float, f, 16, 4);
  VLOAD (vector1, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector1, buffer, , float, f, 32, 2);
  VLOAD(vector1, buffer, q, float, f, 32, 4);

  /* Choose arbitrary initialization values.  */
  VDUP(vector2, , int, s, 8, 8, 0x11);
  VDUP(vector2, , int, s, 16, 4, 0x22);
  VDUP(vector2, , int, s, 32, 2, 0x33);
  VDUP(vector2, , uint, u, 8, 8, 0x55);
  VDUP(vector2, , uint, u, 16, 4, 0x66);
  VDUP(vector2, , uint, u, 32, 2, 0x77);
  VDUP(vector2, , poly, p, 8, 8, 0x55);
  VDUP(vector2, , poly, p, 16, 4, 0x66);
  MFLOAT8_ONLY(VDUP(vector2, , mfloat, mf, 8, 8, MFLOAT8(0xaa)));
#if defined (FP16_SUPPORTED)
  VDUP (vector2, , float, f, 16, 4, 14.6f);   /* 14.6f is 0x4b4d.  */
#endif
  VDUP(vector2, , float, f, 32, 2, 33.6f);

  VDUP(vector2, q, int, s, 8, 16, 0x11);
  VDUP(vector2, q, int, s, 16, 8, 0x22);
  VDUP(vector2, q, int, s, 32, 4, 0x33);
  VDUP(vector2, q, uint, u, 8, 16, 0x55);
  VDUP(vector2, q, uint, u, 16, 8, 0x66);
  VDUP(vector2, q, uint, u, 32, 4, 0x77);
  VDUP(vector2, q, poly, p, 8, 16, 0x55);
  VDUP(vector2, q, poly, p, 16, 8, 0x66);
  MFLOAT8_ONLY(VDUP(vector2, q, mfloat, mf, 8, 16, MFLOAT8(0xbc)));
#if defined (FP16_SUPPORTED)
  VDUP (vector2, q, float, f, 16, 8, 14.6f);
#endif
  VDUP(vector2, q, float, f, 32, 4, 33.8f);

#define TEST_ALL_VSHUFFLE(INSN)				\
  TEST_VSHUFFLE(INSN, , int, s, 8, 8);			\
  TEST_VSHUFFLE(INSN, , int, s, 16, 4);			\
  TEST_VSHUFFLE(INSN, , int, s, 32, 2);			\
  TEST_VSHUFFLE(INSN, , uint, u, 8, 8);			\
  TEST_VSHUFFLE(INSN, , uint, u, 16, 4);		\
  TEST_VSHUFFLE(INSN, , uint, u, 32, 2);		\
  TEST_VSHUFFLE(INSN, , poly, p, 8, 8);			\
  TEST_VSHUFFLE(INSN, , poly, p, 16, 4);		\
  MFLOAT8_ONLY(TEST_VSHUFFLE(INSN, , mfloat, mf, 8, 8)); \
  TEST_VSHUFFLE(INSN, , float, f, 32, 2);		\
  TEST_VSHUFFLE(INSN, q, int, s, 8, 16);		\
  TEST_VSHUFFLE(INSN, q, int, s, 16, 8);		\
  TEST_VSHUFFLE(INSN, q, int, s, 32, 4);		\
  TEST_VSHUFFLE(INSN, q, uint, u, 8, 16);		\
  TEST_VSHUFFLE(INSN, q, uint, u, 16, 8);		\
  TEST_VSHUFFLE(INSN, q, uint, u, 32, 4);		\
  TEST_VSHUFFLE(INSN, q, poly, p, 8, 16);		\
  TEST_VSHUFFLE(INSN, q, poly, p, 16, 8);		\
  MFLOAT8_ONLY(TEST_VSHUFFLE(INSN, q, mfloat, mf, 8, 16)); \
  TEST_VSHUFFLE(INSN, q, float, f, 32, 4)

#define TEST_VSHUFFLE_FP16(INSN)		\
  TEST_VSHUFFLE(INSN, , float, f, 16, 4);	\
  TEST_VSHUFFLE(INSN, q, float, f, 16, 8);

#define TEST_ALL_EXTRA_CHUNKS()			\
  TEST_EXTRA_CHUNK(int, 8, 8, 1);		\
  TEST_EXTRA_CHUNK(int, 16, 4, 1);		\
  TEST_EXTRA_CHUNK(int, 32, 2, 1);		\
  TEST_EXTRA_CHUNK(uint, 8, 8, 1);		\
  TEST_EXTRA_CHUNK(uint, 16, 4, 1);		\
  TEST_EXTRA_CHUNK(uint, 32, 2, 1);		\
  TEST_EXTRA_CHUNK(poly, 8, 8, 1);		\
  TEST_EXTRA_CHUNK(poly, 16, 4, 1);		\
  MFLOAT8_ONLY(TEST_EXTRA_CHUNK(mfloat, 8, 8, 1)); \
  TEST_EXTRA_CHUNK(float, 32, 2, 1);		\
  TEST_EXTRA_CHUNK(int, 8, 16, 1);		\
  TEST_EXTRA_CHUNK(int, 16, 8, 1);		\
  TEST_EXTRA_CHUNK(int, 32, 4, 1);		\
  TEST_EXTRA_CHUNK(uint, 8, 16, 1);		\
  TEST_EXTRA_CHUNK(uint, 16, 8, 1);		\
  TEST_EXTRA_CHUNK(uint, 32, 4, 1);		\
  TEST_EXTRA_CHUNK(poly, 8, 16, 1);		\
  TEST_EXTRA_CHUNK(poly, 16, 8, 1);		\
  MFLOAT8_ONLY(TEST_EXTRA_CHUNK(mfloat, 8, 16, 1)); \
  TEST_EXTRA_CHUNK(float, 32, 4, 1)

  /* vshuffle support all vector types except [u]int64x1 and
     [u]int64x2.  */
#define CHECK_RESULTS_VSHUFFLE(test_name,EXPECTED,comment)		\
  {									\
    CHECK(test_name, int, 8, 8, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, int, 16, 4, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, int, 32, 2, PRIx32, EXPECTED, comment);		\
    CHECK(test_name, uint, 8, 8, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, uint, 16, 4, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, uint, 32, 2, PRIx32, EXPECTED, comment);		\
    CHECK_POLY(test_name, poly, 8, 8, PRIx8, EXPECTED, comment);		\
    CHECK_POLY(test_name, poly, 16, 4, PRIx16, EXPECTED, comment);		\
    MFLOAT8_ONLY(CHECK_FP(test_name, mfloat, 8, 8, PRIx8, EXPECTED, comment)); \
    CHECK_FP(test_name, float, 32, 2, PRIx32, EXPECTED, comment);	\
									\
    CHECK(test_name, int, 8, 16, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, int, 16, 8, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, int, 32, 4, PRIx32, EXPECTED, comment);		\
    CHECK(test_name, uint, 8, 16, PRIx8, EXPECTED, comment);		\
    CHECK(test_name, uint, 16, 8, PRIx16, EXPECTED, comment);		\
    CHECK(test_name, uint, 32, 4, PRIx32, EXPECTED, comment);		\
    CHECK_POLY(test_name, poly, 8, 16, PRIx8, EXPECTED, comment);		\
    CHECK_POLY(test_name, poly, 16, 8, PRIx16, EXPECTED, comment);		\
    MFLOAT8_ONLY(CHECK_FP(test_name, mfloat, 8, 16, PRIx8, EXPECTED, comment)); \
    CHECK_FP(test_name, float, 32, 4, PRIx32, EXPECTED, comment);	\
  }

#define CHECK_RESULTS_VSHUFFLE_FP16(test_name,EXPECTED,comment)		\
  {									\
    CHECK_FP (test_name, float, 16, 4, PRIx16, EXPECTED, comment);	\
    CHECK_FP (test_name, float, 16, 8, PRIx16, EXPECTED, comment);	\
  }

  clean_results ();

  /* Execute the tests.  */
  TEST_ALL_VSHUFFLE(INSN_NAME);
#if defined (FP16_SUPPORTED)
  TEST_VSHUFFLE_FP16 (INSN_NAME);
#endif

  CHECK_RESULTS_VSHUFFLE (TEST_MSG, expected0, "(chunk 0)");
#if defined (FP16_SUPPORTED)
  CHECK_RESULTS_VSHUFFLE_FP16 (TEST_MSG, expected0, "(chunk 0)");
#endif

  TEST_ALL_EXTRA_CHUNKS();
#if defined (FP16_SUPPORTED)
  TEST_EXTRA_CHUNK (float, 16, 4, 1);
  TEST_EXTRA_CHUNK (float, 16, 8, 1);
#endif

  CHECK_RESULTS_VSHUFFLE (TEST_MSG, expected1, "(chunk 1)");
#if defined (FP16_SUPPORTED)
  CHECK_RESULTS_VSHUFFLE_FP16 (TEST_MSG, expected1, "(chunk 1)");
#endif
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
