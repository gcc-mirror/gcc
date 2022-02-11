/* { dg-options "-fpermissive -O2 -fno-indirect-inlining -fno-devirtualize-speculatively -fdump-tree-fre3-details -fno-early-inlining"  } */
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

__attribute__ ((always_inline))
inline
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
/* { dg-final { scan-tree-dump "No dynamic type change found."  "fre3"  } } */
/* { dg-final { scan-tree-dump "Checking vtbl store:"  "fre3"  } } */
/* { dg-final { scan-tree-dump "Function call may change dynamic type:extcall"  "fre3"  } } */
/* { dg-final { scan-tree-dump "converting indirect call to function virtual void"  "fre3" { target { ! implicit_constexpr } } } } */
