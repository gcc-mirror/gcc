/* { dg-options "-fpermissive -fno-indirect-inlining -fno-devirtualize-speculatively -fdump-tree-fre2-details"  } */
#include <stdlib.h>
struct A {virtual void test() {abort ();}};
struct B:A
   {virtual void test() {}
    B();
    B(void (*test)(struct A *));};

void extcall(void);

inline void tt(struct A *a)
{
  a->test();
}

B::B (void (*test)(struct A *))
{
  struct B c;
  struct A *a=this;
  extcall();
  test(a);
}
void
t()
{
  struct B b(tt);
}
/* After inlining the call within constructor needs to be checked to not go into a basetype.
   We should see the vtbl store and we should notice extcall as possibly clobbering the
   type but ignore it because b is in static storage.  */
/* { dg-final { scan-tree-dump "Determined dynamic type."  "fre2"  } } */
/* { dg-final { scan-tree-dump "Checking vtbl store:"  "fre2"  } } */
/* { dg-final { scan-tree-dump "Function call may change dynamic type:extcall"  "fre2"  } } */
/* { dg-final { scan-tree-dump "converting indirect call to function virtual void"  "fre2"  } } */
/* { dg-final { cleanup-tree-dump "fre2" } } */
