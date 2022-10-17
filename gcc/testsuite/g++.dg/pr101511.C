// { dg-do compile }
// { dg-options "-O2 -Wno-div-by-zero" }

void __assert_fail(const char *, const char *, int, const char *)
    __attribute__((__noreturn__));
template <typename T> void test_uint() {
  long __trans_tmp_3, __trans_tmp_1;
  int Error;
  for (;;) {
    {
      unsigned long Tmp = -1;
      __trans_tmp_3 = Tmp - Tmp % 0;
    }
    Error += 0 == __trans_tmp_3 ? 0 : 1;
    !Error ? void() : __assert_fail("", "", 3, __PRETTY_FUNCTION__);
    T Tmp = -1;
    __trans_tmp_1 = Tmp - Tmp % 0;
    Error += 0 == __trans_tmp_1 ? 0 : 1;
    !Error ? void() : __assert_fail("", "", 7, __PRETTY_FUNCTION__);
  }
}
void test() { test_uint<unsigned long>(); }
