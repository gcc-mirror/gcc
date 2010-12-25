// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
// Test that old declaration is used, if new one has no attributes.
extern void __attribute__((stdcall)) foo (int);

void
bar (void)
{
  foo (1);
}

void foo (int i)
{
}


