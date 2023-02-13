/* { dg-do compile } */
/* { dg-options "-O3" } */

void __throw_out_of_range_fmt(...);
char *_M_p;
struct Trans_NS___cxx11_basic_string {
  long _M_string_length;
  long _M_check___pos;
  Trans_NS___cxx11_basic_string() {
    long __length = 0;
    _M_string_length = __length;
  }
  long size() { return _M_string_length; }
  long foo___pos;
  char foo() { return _M_p[foo___pos]; }
  int compare() { __throw_out_of_range_fmt(_M_check___pos, _M_string_length); __builtin_trap(); }
};
bool str_starts_with(Trans_NS___cxx11_basic_string &str,
                     Trans_NS___cxx11_basic_string prefix) {
  if (str.size() < prefix.size())
    str.compare();
  for (; prefix.size();) {
    char __trans_tmp_2 = prefix.foo();
    if (__trans_tmp_2)
      return false;
  }
  __builtin_trap();
}
void testStartsWith() {
  Trans_NS___cxx11_basic_string s1, s2;
  str_starts_with(s1, s2);
}
