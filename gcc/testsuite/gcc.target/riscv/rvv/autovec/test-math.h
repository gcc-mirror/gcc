#define TEST_CEIL(TYPE, CALL) \
  void test_##TYPE##_##CALL (TYPE *out, TYPE *in, unsigned count) \
  {                                                               \
    for (unsigned i = 0; i < count; i++)                          \
      out[i] = CALL (in[i]);                                      \
  }

#define TEST_COND_CEIL(TYPE, CALL)                                           \
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
