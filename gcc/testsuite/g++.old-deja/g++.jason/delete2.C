// { dg-do assemble  }
// PRMS Id: 5003
// Bug: g++ complains about calling the destructor for a const object.

struct A {
public:
  ~A();
};

const A foo ();

void bar()
{
  A n;
  n = foo();		// { dg-bogus "" } deleting const
}
