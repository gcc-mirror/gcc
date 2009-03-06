// PR debug/39372
// { dg-do compile }
// { dg-options "-O0 -g -dA" }
// { dg-final { scan-assembler "DW_OP_addr\[^\n\r\]*\[\n\r\]*\[^\n\r\]*staticvar1" } }
// { dg-final { scan-assembler "DW_OP_addr\[^\n\r\]*\[\n\r\]*\[^\n\r\]*staticvar2" } }

extern void f (int *);

struct A
{
  A(int i);
  void foo(int i);
};

A::A(int i)
{
  static int *staticvar1 = new int(i);
  f (staticvar1);
}

void A::foo(int i)
{
  static int *staticvar2 = new int(i);
  f (staticvar2);
}

void f (int *)
{
}

int
main (void)
{
  A a(42);
  a.foo(42);
  return 0;
}
