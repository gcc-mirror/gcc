// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

export import :baz;

export int foo (int a)
{
  return a;
}
