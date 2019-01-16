// { dg-additional-options -fmodules-ts }
export module foo:bar;
// { dg-module-bmi foo:bar }

int foo (int x)
{
  return -x;
}
