// { dg-module-do run }

export module One;
// { dg-module-bmi "One" }

export int Frob (int a)
{
  return -a;
}
