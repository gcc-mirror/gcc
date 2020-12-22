// { dg-additional-options {-fmodule-header -nostdinc} }
// { dg-module-cmi {} }

#define MACRO(X) X

inline int frob (int x)
{
  return x + 2;
}
