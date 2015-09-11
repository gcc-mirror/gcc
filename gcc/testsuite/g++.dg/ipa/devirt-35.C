/* { dg-options "-O2 -fdump-ipa-devirt-details -fdump-tree-fre1-details"  } */
struct A {virtual int t(void) {return 1;}};
struct B:A {B(); virtual int t(void) {return 2;}};
void test2(struct A *);
int
m(struct B *b)
{
  struct A *a = new (B);
  a->t(); // This call should be devirtualized by 
          // FRE because we know type from ctor call
  ((struct B *)a)->B::t(); // Make devirt possible 
                           // C++ FE won't produce inline body without this
  test2(a);
  return a->t();  // This call should be devirtualized speculatively because
                  //  test2 may change the type of A by placement new.
                  // C++ standard is bit imprecise about this.
}
/* { dg-final { scan-tree-dump "converting indirect call to function virtual int B::t"  "fre1"  } } */
/* { dg-final { scan-ipa-dump "to virtual int B::t"  "devirt"  } } */
/* { dg-final { scan-ipa-dump "1 speculatively devirtualized"  "devirt"  } } */

