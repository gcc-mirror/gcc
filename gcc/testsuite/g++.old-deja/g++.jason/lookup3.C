// [class.ambig]: A single function, object, type, or enumerator may be
// reached through more than one path through the DAG of base classes.  This
// is not an ambiguity.
// Build don't link:

struct A  {
  typedef long T;
};

struct B : public A { };
struct C : public A { };

struct D : public C , public B {
  void f (T&);			// gets bogus error - ambiguous lookup
};
