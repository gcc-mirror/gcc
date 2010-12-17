// PR target/20795
// Test passing aligned empty aggregate
// { dg-do compile }
// { dg-options "-Wno-psabi" { target { { i?86-*-* x86_64-*-* } && ilp32 } } }

struct S { union {} a; } __attribute__((aligned));

S
foo (S arg)
{
  return arg;
}

void
bar (void)
{
  S arg;
  foo (arg);
}
