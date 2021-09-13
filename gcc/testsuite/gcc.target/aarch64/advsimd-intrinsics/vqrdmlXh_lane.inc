#define FNNAME1(NAME) exec_ ## NAME ## _lane
#define FNNAME(NAME) FNNAME1 (NAME)

void FNNAME (INSN) (void)
{
  /* vector_res = vqrdmlXh_lane (vector, vector2, vector3, lane),
     then store the result.  */
#define TEST_VQRDMLXH_LANE2(INSN, Q, T1, T2, W, N, N2, L,		\
			    CMT)		\
  Set_Neon_Cumulative_Sat (0, VECT_VAR (vector_res, T1, W, N));		\
  VECT_VAR (vector_res, T1, W, N) =					\
    INSN##Q##_lane_##T2##W (VECT_VAR (vector, T1, W, N),		\
			    VECT_VAR (vector2, T1, W, N),		\
			    VECT_VAR (vector3, T1, W, N2),		\
			    L);						\
  vst1##Q##_##T2##W (VECT_VAR (result, T1, W, N),			\
		     VECT_VAR (vector_res, T1, W, N))

  /* Two auxliary macros are necessary to expand INSN.  */
#define TEST_VQRDMLXH_LANE1(INSN, Q, T1, T2, W, N, N2, L,	\
			    CMT)	\
  TEST_VQRDMLXH_LANE2 (INSN, Q, T1, T2, W, N, N2, L,		\
		       CMT)

#define TEST_VQRDMLXH_LANE(Q, T1, T2, W, N, N2, L,		\
			   CMT)	\
  TEST_VQRDMLXH_LANE1 (INSN, Q, T1, T2, W, N, N2, L,		\
		       CMT)


  DECL_VARIABLE (vector, int, 16, 4);
  DECL_VARIABLE (vector, int, 32, 2);
  DECL_VARIABLE (vector, int, 16, 8);
  DECL_VARIABLE (vector, int, 32, 4);

  DECL_VARIABLE (vector_res, int, 16, 4);
  DECL_VARIABLE (vector_res, int, 32, 2);
  DECL_VARIABLE (vector_res, int, 16, 8);
  DECL_VARIABLE (vector_res, int, 32, 4);

  DECL_VARIABLE (vector2, int, 16, 4);
  DECL_VARIABLE (vector2, int, 32, 2);
  DECL_VARIABLE (vector2, int, 16, 8);
  DECL_VARIABLE (vector2, int, 32, 4);

  DECL_VARIABLE (vector3, int, 16, 4);
  DECL_VARIABLE (vector3, int, 32, 2);
  DECL_VARIABLE (vector3, int, 16, 8);
  DECL_VARIABLE (vector3, int, 32, 4);

  clean_results ();

  VLOAD (vector, buffer, , int, s, 16, 4);
  VLOAD (vector, buffer, , int, s, 32, 2);

  VLOAD (vector, buffer, q, int, s, 16, 8);
  VLOAD (vector, buffer, q, int, s, 32, 4);

  /* Initialize vector2.  */
  VDUP (vector2, , int, s, 16, 4, 0x5555);
  VDUP (vector2, , int, s, 32, 2, 0xBB);
  VDUP (vector2, q, int, s, 16, 8, 0xBB);
  VDUP (vector2, q, int, s, 32, 4, 0x22);

  /* Initialize vector3.  */
  VDUP (vector3, , int, s, 16, 4, 0x5555);
  VDUP (vector3, , int, s, 32, 2, 0xBB);
  VDUP (vector3, q, int, s, 16, 8, 0x33);
  VDUP (vector3, q, int, s, 32, 4, 0x22);

  /* Choose lane arbitrarily.  */
#define CMT ""
  TEST_VQRDMLXH_LANE (, int, s, 16, 4, 4, 2, CMT);
  TEST_VQRDMLXH_LANE (, int, s, 32, 2, 2, 1, CMT);
  TEST_VQRDMLXH_LANE (q, int, s, 16, 8, 4, 3, CMT);
  TEST_VQRDMLXH_LANE (q, int, s, 32, 4, 2, 0, CMT);

  CHECK (TEST_MSG, int, 16, 4, PRIx16, expected, CMT);
  CHECK (TEST_MSG, int, 32, 2, PRIx32, expected, CMT);
  CHECK (TEST_MSG, int, 16, 8, PRIx16, expected, CMT);
  CHECK (TEST_MSG, int, 32, 4, PRIx32, expected, CMT);

  /* Now use input values such that the multiplication causes
     saturation.  */
#define TEST_MSG_MUL " (check mul cumulative saturation)"
  VDUP (vector, , int, s, 16, 4, 0x8000);
  VDUP (vector, , int, s, 32, 2, 0x80000000);
  VDUP (vector, q, int, s, 16, 8, 0x8000);
  VDUP (vector, q, int, s, 32, 4, 0x80000000);

  VDUP (vector2, , int, s, 16, 4, 0x8000);
  VDUP (vector2, , int, s, 32, 2, 0x80000000);
  VDUP (vector2, q, int, s, 16, 8, 0x8000);
  VDUP (vector2, q, int, s, 32, 4, 0x80000000);

  VDUP (vector3, , int, s, 16, 4, 0x8000);
  VDUP (vector3, , int, s, 32, 2, 0x80000000);
  VDUP (vector3, q, int, s, 16, 8, 0x8000);
  VDUP (vector3, q, int, s, 32, 4, 0x80000000);

  TEST_VQRDMLXH_LANE (, int, s, 16, 4, 4, 2, TEST_MSG_MUL);
  TEST_VQRDMLXH_LANE (, int, s, 32, 2, 2, 1, TEST_MSG_MUL);
  TEST_VQRDMLXH_LANE (q, int, s, 16, 8, 4, 3, TEST_MSG_MUL);
  TEST_VQRDMLXH_LANE (q, int, s, 32, 4, 2, 0, TEST_MSG_MUL);

  CHECK (TEST_MSG, int, 16, 4, PRIx16, expected_mul, TEST_MSG_MUL);
  CHECK (TEST_MSG, int, 32, 2, PRIx32, expected_mul, TEST_MSG_MUL);
  CHECK (TEST_MSG, int, 16, 8, PRIx16, expected_mul, TEST_MSG_MUL);
  CHECK (TEST_MSG, int, 32, 4, PRIx32, expected_mul, TEST_MSG_MUL);

  VDUP (vector, , int, s, 16, 4, 0x8000);
  VDUP (vector, , int, s, 32, 2, 0x80000000);
  VDUP (vector, q, int, s, 16, 8, 0x8000);
  VDUP (vector, q, int, s, 32, 4, 0x80000000);

  VDUP (vector2, , int, s, 16, 4, 0x8001);
  VDUP (vector2, , int, s, 32, 2, 0x80000001);
  VDUP (vector2, q, int, s, 16, 8, 0x8001);
  VDUP (vector2, q, int, s, 32, 4, 0x80000001);

  VDUP (vector3, , int, s, 16, 4, 0x8001);
  VDUP (vector3, , int, s, 32, 2, 0x80000001);
  VDUP (vector3, q, int, s, 16, 8, 0x8001);
  VDUP (vector3, q, int, s, 32, 4, 0x80000001);

  /* Use input values where rounding produces a result equal to the
     saturation value, but does not set the saturation flag.  */
#define TEST_MSG_ROUND " (check rounding)"
  TEST_VQRDMLXH_LANE (, int, s, 16, 4, 4, 2, TEST_MSG_ROUND);
  TEST_VQRDMLXH_LANE (, int, s, 32, 2, 2, 1, TEST_MSG_ROUND);
  TEST_VQRDMLXH_LANE (q, int, s, 16, 8, 4, 3, TEST_MSG_ROUND);
  TEST_VQRDMLXH_LANE (q, int, s, 32, 4, 2, 0, TEST_MSG_ROUND);

  CHECK (TEST_MSG, int, 16, 4, PRIx16, expected_round, TEST_MSG_ROUND);
  CHECK (TEST_MSG, int, 32, 2, PRIx32, expected_round, TEST_MSG_ROUND);
  CHECK (TEST_MSG, int, 16, 8, PRIx16, expected_round, TEST_MSG_ROUND);
  CHECK (TEST_MSG, int, 32, 4, PRIx32, expected_round, TEST_MSG_ROUND);
}

int
main (void)
{
  FNNAME (INSN) ();
  return 0;
}
