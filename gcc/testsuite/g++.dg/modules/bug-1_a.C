// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }
export module One;
// { dg-module-bmi "One" }

export int Frob (int a)
{
  return -a;
}
