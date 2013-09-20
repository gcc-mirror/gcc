#define B_TEST(TYPE) { TYPE v __attribute__((vector_size(16))); (void)((v < v) < v); }
#ifdef __cplusplus
#define T_TEST(TYPE) { TYPE s; TYPE v __attribute__((vector_size(16))); __typeof((v<v)[0]) iv __attribute__((vector_size(16))); (void)((iv ? s : s) < v); }
#else
#define T_TEST(TYPE)
#endif
#define T(TYPE) B_TEST(TYPE) T_TEST(TYPE)

void f ()
{
  T(short)
  T(int)
  T(long)
  T(long long)

  T_TEST(float)
  T_TEST(double)
  /* Avoid trouble with non-power-of-two sizes.  */
#if !defined(__i386__) && !defined(__x86_64__) && !defined(__m68k__) && !defined(__ia64__)
  T_TEST(long double)
#endif
}
