/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-inline-details -fno-early-inlining -fno-ipa-cp" } */
struct A {virtual int foo () {return 1;}};
struct B:A {virtual int foo () {return 2;}};

void dostuff(struct A *);

static void
test (struct A *a)
{
  dostuff (a);
  if (a->foo ()!= 2)
    __builtin_abort ();
}

int main()
{
  struct B a;
  dostuff (&a);
  test (&a);
}
/* Inlining of dostuff into main should combine polymorphic context
   specifying Outer type:struct B offset 0
   with Outer type (dynamic):struct A (or a derived type) offset 0
   and enable devirtualization.

   Because the type is in static storage, we know it won't change type in dostuff
   and from callstack we can tell that is is not in construction/destruction.  */
/* { dg-final { scan-ipa-dump "Second type is base of first" "inline"  } } */
/* { dg-final { scan-ipa-dump-times "Discovered a virtual call to a known target" 1 "inline"  } } */
