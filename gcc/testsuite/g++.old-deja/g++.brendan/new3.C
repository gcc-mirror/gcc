// This test fails on VxWorks in kernel mode because it depends on the
// library version of "::operator new[]" calling the "::operator new"
// defined in this module.  This doesn't work because the library version
// of "::operator new[]" is built into the kernel itself; library relocations
// are resolved when the kernel is linked.
// { dg-do run { xfail { powerpc-ibm-aix* || vxworks_kernel } } }
// { dg-options "-flat_namespace" { target *-*-darwin[67]* } }
// Avoid use of none-overridable new/delete operators in shared
// { dg-options "-static" { target *-*-mingw* } }
// { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } }

// GROUPS passed operator-new
#include <stdio.h>
#include <stdlib.h>
#include <new>

int pass = 0;

void *operator new(size_t sz)
#if __cplusplus <= 199711L
  throw (std::bad_alloc)
#endif
{

  void *p;

  pass = 1;
  p = malloc(sz);
  return p;
}

class A {
public:
  A() {}
  ~A() {}

  int a;
  int b;
};


int main()
{
  A *bb = new A[10];
  delete [] bb;

  if (pass)
    printf ("PASS\n");
  else
    { printf ("FAIL\n"); return 1; }
}
