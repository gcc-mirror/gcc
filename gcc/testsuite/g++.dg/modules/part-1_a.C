// { dg-additional-options -fmodules-ts }
// { dg-module-cmi foo:baz }

export module foo:baz;

export int baz ()
{
  return -1;
}
