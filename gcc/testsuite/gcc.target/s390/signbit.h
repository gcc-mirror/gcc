#define TEST(T, U, I, N, C0, C42)                                              \
  void test_##T (void)                                                         \
  {                                                                            \
    U tmp;                                                                     \
    int x;                                                                     \
                                                                               \
    x = signbit_##T##_reg(C42);                                                \
    x += signbit_##T##_reg(C0);                                                \
    x += signbit_##T##_reg(I);                                                 \
    x += signbit_##T##_reg(N);                                                 \
    tmp = C42;                                                                 \
    x += signbit_##T##_mem(&tmp);                                              \
    tmp = C0;                                                                  \
    x += signbit_##T##_mem(&tmp);                                              \
    tmp = I;                                                                   \
    x += signbit_##T##_mem(&tmp);                                              \
    tmp = N;                                                                   \
    x += signbit_##T##_mem(&tmp);                                              \
    if (x != 0)                                                                \
      __builtin_abort();                                                       \
                                                                               \
    x = signbit_##T##_reg(-C42);                                               \
    x += signbit_##T##_reg(-C0);                                               \
    x += signbit_##T##_reg(-I);                                                \
    x += signbit_##T##_reg(-N);                                                \
    tmp = -C42;                                                                \
    x += signbit_##T##_mem(&tmp);                                              \
    tmp = -C0;                                                                 \
    x += signbit_##T##_mem(&tmp);                                              \
    tmp = -I;                                                                  \
    x += signbit_##T##_mem(&tmp);                                              \
    tmp = -N;                                                                  \
    x += signbit_##T##_mem(&tmp);                                              \
    if (x != 8)                                                                \
      __builtin_abort();                                                       \
  }
