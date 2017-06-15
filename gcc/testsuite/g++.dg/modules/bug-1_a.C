// { dg-module-do run }

export module One;
// { dg-module-if "One" }

export int Frob (int a)
{
  return -a;
}
