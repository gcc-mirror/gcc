// PR target/102024
// { dg-do compile }
// { dg-options "-O2 -mabi=64 -mhard-float" }
// { dg-final { scan-assembler-not "\\\$f0" } }

struct foo
{
  double a;
  int : 0;
  double b;
};

struct foo
make_foo(void) // { dg-message "the ABI for returning a value containing zero-width bit-fields but otherwise an aggregate with only one or two floating-point fields was changed in GCC 12.1" }
{
  struct foo ret;
  ret.a = 114;
  ret.b = 514;
  return ret;
}
