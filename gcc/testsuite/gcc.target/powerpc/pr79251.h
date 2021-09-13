
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

TEST_VEC_INSERT_ALL (test)

#define run_test(TYPE, num)                                                    \
  {                                                                            \
    vector TYPE v;                                                             \
    vector TYPE u = {0x0};                                                     \
    for (long k = 0; k < 16 / sizeof (TYPE); k++)                              \
      v[k] = 0xaa;                                                             \
    for (long k = 0; k < 16 / sizeof (TYPE); k++)                              \
      {                                                                        \
	u = test##num (v, 254, k);                                             \
	if (u[k] != (TYPE) 254)                                                \
	  __builtin_abort ();                                                  \
      }                                                                        \
  }

