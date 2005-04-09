// PR target/20795
// Test passing aligned empty aggregate
// { dg-do compile }

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
