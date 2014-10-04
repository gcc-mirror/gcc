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
   namespace type and therefore we know it has no derivations.
   FIXME: Currently we devirtualize speculatively only because we do not track
   dynamic type changes well.  */
/* { dg-final { scan-ipa-dump-times "First type is base of second" 1 "inline"  } } */
/* { dg-final { scan-ipa-dump-times "Outer types match, merging flags" 2 "inline"  } } */
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target" 1 "inline"  } } */
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a speculative target" 1 "inline"  } } */

/* Verify that speculation is optimized by late optimizers.  */
/* { dg-final { scan-ipa-dump-times "return 2" 2 "optimized"  } } */
/* { dg-final { scan-ipa-dump-not "OBJ_TYPE_REF" "optimized"  } } */

/* { dg-final { cleanup-ipa-dump "inline" } } */
/* { dg-final { cleanup-ipa-dump "optimized" } } */
