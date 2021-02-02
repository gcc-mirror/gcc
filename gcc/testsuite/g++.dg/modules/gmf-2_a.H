// { dg-module-do run }
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

#define MACRO(X) X

inline int frob (int x)
{
  return x + 2;
}
