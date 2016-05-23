#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1 (NAME)

void FNNAME (INSN) (void)
{
  /* vector_res = vrndX (vector), then store the result.  */
#define TEST_VRND2(INSN, Q, T1, T2, W, N)				\
  VECT_VAR (vector_res, T1, W, N) =					\
    INSN##Q##_##T2##W (VECT_VAR (vector, T1, W, N));			\
    vst1##Q##_##T2##W (VECT_VAR (result, T1, W, N),			\
		       VECT_VAR (vector_res, T1, W, N))

  /* Two auxliary macros are necessary to expand INSN.  */
#define TEST_VRND1(INSN, Q, T1, T2, W, N)	\
  TEST_VRND2 (INSN, Q, T1, T2, W, N)

#define TEST_VRND(Q, T1, T2, W, N)		\
  TEST_VRND1 (INSN, Q, T1, T2, W, N)

  DECL_VARIABLE (vector, float, 32, 2);
  DECL_VARIABLE (vector, float, 32, 4);

  DECL_VARIABLE (vector_res, float, 32, 2);
  DECL_VARIABLE (vector_res, float, 32, 4);

  clean_results ();

  VLOAD (vector, buffer, , float, f, 32, 2);
  VLOAD (vector, buffer, q, float, f, 32, 4);

  TEST_VRND ( , float, f, 32, 2);
  TEST_VRND (q, float, f, 32, 4);

  CHECK_FP (TEST_MSG, float, 32, 2, PRIx32, expected, "");
  CHECK_FP (TEST_MSG, float, 32, 4, PRIx32, expected, "");
}

int
main (void)
{
  FNNAME (INSN) ();
  return 0;
}
