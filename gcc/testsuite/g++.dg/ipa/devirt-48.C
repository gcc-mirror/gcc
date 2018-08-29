/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-cp -fdump-ipa-inline-details -fno-early-inlining" } */
struct A {
   virtual int foo(){return 1;}
};
struct B:A {
   virtual int foo(){return 2;}
   void callfoo(){foo();}
};
struct C:A {
   virtual int foo(){return 3;}
};
struct D:B {
   virtual int foo(){return 4;}
   void callfoo(){foo();}
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
  static_cast<B*>(a)->callfoo();
  return 0;
}

/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target\[^\\n\]*__builtin_unreachable" 1 "inline"  } } */
