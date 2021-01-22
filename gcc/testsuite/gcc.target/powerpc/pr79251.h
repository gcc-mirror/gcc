
#define test(TYPE, num)                                                        \
  __attribute__ ((noinline, noclone))                                          \
    vector TYPE test##num (vector TYPE v, TYPE i, signed int n)                \
  {                                                                            \
    return vec_insert (i, v, n);                                               \
  }

#define TEST_VEC_INSERT_ALL(T)                                                 \
  T (char, 0)                                                                  \
  T (unsigned char, 1)                                                         \
  T (short, 2)                                                                 \
  T (unsigned short, 3)                                                        \
  T (int, 4)                                                                   \
  T (unsigned int, 5)                                                          \
  T (long long, 6)                                                             \
  T (unsigned long long, 7)                                                    \
  T (float, 8)                                                                 \
  T (double, 9)
