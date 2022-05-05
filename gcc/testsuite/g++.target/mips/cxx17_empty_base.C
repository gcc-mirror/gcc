// { dg-do compile }
// { dg-options "-O2 -std=c++17 -mabi=64 -mhard-float" }
// { dg-final { scan-assembler "\\\$f0" } }

struct empty {};

struct foo : empty
{
  double a;
  double b;
};

struct foo
make_foo(void) // { dg-message "the ABI for returning a value with C\\\+\\\+17 empty bases but otherwise an aggregate with only one or two floating-point fields was changed in GCC 12.1" }
{
  struct foo ret;
  ret.a = 114;
  ret.b = 514;
  return ret;
}
