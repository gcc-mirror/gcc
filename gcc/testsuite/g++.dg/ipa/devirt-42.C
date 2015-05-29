/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fdump-ipa-inline-details -fno-early-inlining -fdump-tree-optimized" } */
struct A {
  virtual int foo () {return 1;}
  int bar () {return foo();}
  int barbar ();
};
namespace {
  struct B:A {virtual int foo () {return 2;}
	      int barbar () {return bar();}};
}

int
A::barbar()
{
  return static_cast<B*>(this)->barbar();
}

main()
{
  struct B b;
  struct A *a = &b;
  return a->barbar ();
}

/* Inlining everything into main makes type clear from type of variable b.
   However devirtualization is also possible for offline copy of A::barbar. Invoking
   B's barbar makes it clear the type is at least B and B is an anonymous
   namespace type and therefore we know it has no derivations.  */
/* { dg-final { scan-ipa-dump "First type is base of second" "inline"  } } */
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target" 2 "inline"  } } */

/* Verify that speculation is optimized by late optimizers.  */
/* { dg-final { scan-tree-dump-times "return 2" 3 "optimized"  } } */
/* { dg-final { scan-tree-dump-not "OBJ_TYPE_REF" "optimized"  } } */

