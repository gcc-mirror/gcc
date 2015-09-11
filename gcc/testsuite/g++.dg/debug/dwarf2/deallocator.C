// Test that debug info generated for auto-inserted deallocator is
// correctly attributed.
// This patch scans for the lineno directly from assembly, which may
// differ between different architectures. Because it mainly tests
// FE generated debug info, without losing generality, only x86
// assembly is scanned in this test.
// { dg-do compile { target { i?86-*-* x86_64-*-* } } }
// { dg-options "-O2 -fno-exceptions -gdwarf-2 -dA" }

struct t {
  t ();
  ~t ();
  void foo();
  void bar();
};

int bar();

void foo(int i)
{
  t test_outside;
  for (int j = 0; j < 10; j++)
    {
      t test;
      test.foo();
      if (i + j)
	{
	  test.bar();
	  return;
	}
    }
  if (i)
    {
      t test;
      if (i == 10)
	{
	  test.bar();
	}
    }
  test_outside.foo();
  return;
}
// { dg-final { scan-assembler "deallocator.C:29" } }
// { dg-final { scan-assembler "deallocator.C:24" } }
// { dg-final { scan-assembler "deallocator.C:34" } }
// { dg-final { scan-assembler "deallocator.C:21" } }
