// PR c++/10091

// Bug: We were dying because in general, B::a doesn't have enough
// alignment for us to take its address.  But if the B is C::b, it does
// have enough alignment, and we should be able to determine that.

// This only failed on STRICT_ALIGNMENT targets (i.e. not i686)

struct A {
  int i;

  A();
  A(const A&);
  A& operator=(const A&);
};

struct B {
  A a;
} __attribute__ ((packed));

struct C {
  B b;
  int j;
};

void f (const A&);
void g (const C& c)
{
  f (c.b.a);
}
