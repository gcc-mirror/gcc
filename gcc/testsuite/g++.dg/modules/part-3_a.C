// { dg-additional-options -fmodules-ts }
export module foo:bar;
// { dg-module-cmi foo:bar }

int foo (int x)
{
  return -x;
}
