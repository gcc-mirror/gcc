/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fdump-ipa-inline-details -fno-early-inlining -fdump-tree-optimized" } */
struct A {
   virtual int foo(){return 1;}
};
struct B {
   virtual int bar(){return 4;}
};
struct C:B,A {
   virtual int foo(){return 2;}
};
static void
test (struct A *a)
{
  if (a->foo() != 2)
   __builtin_abort ();
}
int
m()
{
  struct A *a = new C;
  test (a);
  return 0;
}

/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a speculative target\[^\\n\]*C::_ZTh" 1 "inline"  } } */
/* { dg-final { scan-ipa-dump-not "OBJ_TYPE_REF" "optimized"  } } */
/* FIXME: We ought to inline thunk.  */
/* { dg-final { scan-ipa-dump "C::_ZThn" "optimized"  } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
/* { dg-final { cleanup-ipa-dump "optimized" } } */
