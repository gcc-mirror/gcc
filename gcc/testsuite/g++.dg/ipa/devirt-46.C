/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fdump-ipa-inline-details -fno-early-inlining -fdump-tree-optimized" } */
struct A {
   virtual int foo(){return 1;}
};
struct B:A {
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
  struct A *a = new B;
  test (a);
  return 0;
}

/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target\[^\\n\]*B::foo" 1 "inline"  } } */
/* { dg-final { scan-tree-dump-not "OBJ_TYPE_REF" "optimized"  } } */
/* { dg-final { scan-tree-dump-not "abort" "optimized"  } } */
