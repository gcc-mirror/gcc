// { dg-lto-do link }
// { dg-lto-options { { -O0 -flto -shared -fPIC } } }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }
// { dg-extra-ld-options "-shared" }

#define XXX
#include "pr64076.H"

int main()
{
  S s;
  return 0;
}
