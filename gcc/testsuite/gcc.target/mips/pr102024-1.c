// PR target/102024
// { dg-do compile }
// { dg-options "-mabi=64 -mhard-float" }
// { dg-final { scan-assembler "\\\$f12" } }

struct foo
{
  int : 0;
  double a;
};

extern void func(struct foo);

void
pass_foo(void)
{
  struct foo test;
  test.a = 114;
  func(test); // { dg-message "the ABI for passing a value containing zero-width fields before an adjacent 64-bit floating-point field was changed in GCC 12.1" }
}
