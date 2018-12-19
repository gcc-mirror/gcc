// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-bmi foo }

export import :baz;

export int foo (int a)
{
  return a;
}
