// PRMS Id: 5003
// Bug: g++ complains about calling the destructor for a const object.
// Build don't link:

struct A {
public:
  ~A();
};

const A foo ();

void bar()
{
  A n;
  n = foo();		// gets bogus error - deleting const
}
