// { dg-module-do run }

export module One;
// { dg-module-if "One" }

export struct X
{
  X (int, int);
  int a;
  int b;
};
