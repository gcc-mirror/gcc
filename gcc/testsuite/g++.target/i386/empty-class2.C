// PR middle-end/101160
// Test passing aligned empty aggregate
// { dg-do compile }
// { dg-options "-O2" }

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
