#define TEST(name, float_type, int_type, pref) void f_##name (float_type x) \
{									    \
  volatile float_type a = __builtin_rint (x);				    \
  volatile int_type   b = __builtin_l##pref##rint (x);			    \
}
