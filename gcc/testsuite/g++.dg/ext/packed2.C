// PR c++/10091

// Original synopsis
// Bug: We were dying because in general, B::a doesn't have enough
// alignment for us to take its address.  But if the B is C::b, it does
// have enough alignment, and we should be able to determine that.

// This only failed on STRICT_ALIGNMENT targets (i.e. not i686)

// July 2003
// packing of non-pods is now only allowed if the non-pod is itself
// packed. Also only such pods can be reference bound to non-consts

struct A {
  int i;

  A();
  A(const A&);
  A& operator=(const A&);
} __attribute__ ((packed));

struct B {
  A a;
} __attribute__ ((packed));

struct C {
  B b;
  int j;
};

void f (A&);
void g (C& c)
{
  f (c.b.a);
}
