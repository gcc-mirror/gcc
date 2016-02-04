#define PASS_ARRAY(...) {__VA_ARGS__}

#define SETUP_TEST_CASE_VEC(I, INTRINSIC, BASE_TYPE, TYPE1, TYPE2,	\
			    VALS1, VALS2, EXPS, LEN, FM, Q_LD, Q_ST,	\
			    V1, V2)					\
  do									\
    {									\
  int i##I;								\
  BASE_TYPE vec##I##_1_data[] = VALS1;					\
  BASE_TYPE vec##I##_2_data[] = VALS2;					\
  V1 TYPE1 vec##I##_1 = vld1##Q_LD##_##FM (vec##I##_1_data);		\
  V2 TYPE2 vec##I##_2 = vld1##Q_LD##_##FM (vec##I##_2_data);		\
  TYPE1 actual##I##_v = INTRINSIC (vec##I##_1, vec##I##_2);		\
  volatile BASE_TYPE expected##I[] = EXPS;				\
  BASE_TYPE actual##I[LEN];						\
  vst1##Q_ST##_##FM (actual##I, actual##I##_v);				\
  for (i##I = 0; i##I < LEN; ++i##I)					\
    if (actual##I[i##I] != expected##I[i##I])				\
      abort ();								\
    }									\
  while (0)								\

#define SETUP_TEST_CASE_SCALAR(I, INTRINSIC, TYPE, VAL1, VAL2, EXP)	\
  do									\
    {									\
  TYPE vec_##I##_1 = VAL1;						\
  TYPE vec_##I##_2 = VAL2;						\
  TYPE expected_##I = EXP;						\
  volatile TYPE actual_##I = INTRINSIC (vec_##I##_1, vec_##I##_2);	\
  if (actual_##I != expected_##I)					\
    abort ();								\
    }									\
  while (0)								\

/* Functions used to return values that won't be optimised away.  */
float32_t  __attribute__ ((noinline))
foo32 ()
{
  return 1.0;
}

float64_t  __attribute__ ((noinline))
foo64 ()
{
  return 1.0;
}
