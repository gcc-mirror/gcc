#define TEST_UNARY_CALL(TYPE, CALL)                               \
  void test_##TYPE##_##CALL (TYPE *out, TYPE *in, unsigned count) \
  {                                                               \
    for (unsigned i = 0; i < count; i++)                          \
      out[i] = CALL (in[i]);                                      \
  }

#define TEST_UNARY_CALL_CVT(TYPE_IN, TYPE_OUT, CALL) \
  void test_##TYPE_IN##_##TYPE_OUT##_##CALL (        \
    TYPE_OUT *out, TYPE_IN *in, unsigned count)      \
  {                                                  \
    for (unsigned i = 0; i < count; i++)             \
      out[i] = CALL (in[i]);                         \
  }

#define TEST_COND_UNARY_CALL(TYPE, CALL)                                     \
  void test_##TYPE##_##CALL (TYPE *out, int *cond, TYPE *in, unsigned count) \
  {                                                                          \
    for (unsigned i = 0; i < count; i++)                                     \
      out[i] = cond[i] ? CALL (in[i]) : in[i];                               \
  }

#define TEST_INIT(TYPE, VAL_IN, VAL_REF, NUM)                        \
  void test_##TYPE##_init_##NUM (TYPE *in, TYPE *ref, unsigned size) \
  {                                                                  \
    for (unsigned i = 0; i < size; i++)                              \
      {                                                              \
	in[i] = VAL_IN;                                              \
	ref[i] = VAL_REF;                                            \
      }                                                              \
  }

#define TEST_INIT_CVT(TYPE_IN, VAL_IN, TYPE_REF, VAL_REF, NUM) \
  void test_##TYPE_IN##_##TYPE_REF##_init_##NUM (              \
    TYPE_IN *in, TYPE_REF *ref, unsigned size)                 \
  {                                                            \
    for (unsigned i = 0; i < size; i++)                        \
      {                                                        \
	in[i] = VAL_IN;                                        \
	ref[i] = VAL_REF;                                      \
      }                                                        \
  }

#define TEST_ASSERT(TYPE)                                         \
  void test_##TYPE##_assert (TYPE *out, TYPE *ref, unsigned size) \
  {                                                               \
    for (unsigned i = 0; i < size; i++)                           \
      {                                                           \
	if (out[i] != ref[i])                                     \
	  __builtin_abort ();                                     \
      }                                                           \
  }

#define RUN_TEST(TYPE, NUM, CALL, IN, OUT, REF, SIZE) \
  test_##TYPE##_init_##NUM (IN, REF, SIZE);           \
  test_##TYPE##_##CALL (OUT, IN, SIZE);               \
  test_##TYPE##_assert (OUT, REF, SIZE);

#define RUN_TEST_CVT(TYPE_IN, TYPE_OUT, NUM, CALL, IN, OUT, REF, SIZE) \
  test_##TYPE_IN##_##TYPE_OUT##_init_##NUM (IN, REF, SIZE);            \
  test_##TYPE_IN##_##TYPE_OUT##_##CALL (OUT, IN, SIZE);                \
  test_##TYPE_OUT##_assert (OUT, REF, SIZE);

#define FRM_RNE 0
#define FRM_RTZ 1
#define FRM_RDN 2
#define FRM_RUP 3
#define FRM_RMM 4
#define FRM_DYN 7

static inline void
set_rm (unsigned rm)
{
  __asm__ volatile (
    "fsrm %0"
    :
    :"r"(rm)
    :
  );
}

static inline unsigned
get_fflags ()
{
  unsigned fflags = 0;

  __asm__ volatile (
    "frflags %0"
    :"=r"(fflags)
    :
    :
  );

  return fflags;
}
