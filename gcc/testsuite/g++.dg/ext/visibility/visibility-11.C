// PR target/39175
// { dg-do compile }
// { dg-require-visibility "" }
// { dg-options "-O2 -fvisibility=hidden -fpic" { target fpic } }

__attribute__((noinline)) int
foo (int x)
{
  return x;
}

int foo (int x);

int
bar (int x)
{
  return foo (x);
}
