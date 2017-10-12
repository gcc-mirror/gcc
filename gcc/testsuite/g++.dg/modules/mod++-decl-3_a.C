// { dg-options "-fmodules++ -fmodule-root=." }
// { dg-module-bmi "mod++-decl-3_a" }
// { dg-module-do run }

export module "mod++-decl-3_a";

int frob (int a)
{
  return a * 2;
}

export inline int bink (int a)
{
  return frob (a);
}
