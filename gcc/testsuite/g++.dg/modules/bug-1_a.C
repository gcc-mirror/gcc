// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }
export module One;
// { dg-module-cmi "One" }

export int Frob (int a)
{
  return -a;
}
