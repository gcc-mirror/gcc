// { dg-do assemble  }
// [class.ambig]: A single function, object, type, or enumerator may be
// reached through more than one path through the DAG of base classes.  This
// is not an ambiguity.

struct A  {
  typedef long T;
};

struct B : public A { };
struct C : public A { };

struct D : public C , public B {
  void f (T&);			// { dg-bogus "" } ambiguous lookup
};
