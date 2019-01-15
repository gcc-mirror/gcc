// { dg-additional-options -fmodules-ts }
// { dg-module-bmi foo:baz }

export module foo:baz;

export int baz ()
{
  return -1;
}
