// { dg-additional-options -fmodules-ts }
// { dg-module-bmi foo:baz }

export module foo:baz;

export int foo ()
{
  return -1;
}
