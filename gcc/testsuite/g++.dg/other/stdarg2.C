// PR c++/11929
// Bug: We were complaining about the call to va_start because o is of
// non-POD type.

struct s {
  s(int);
};

void test(s o, ...) {
  __builtin_va_list varg;
  __builtin_va_start(varg, o);
}
