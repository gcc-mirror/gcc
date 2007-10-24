// DR 391 says that we always bind a reference to the base subobject; it is
// incorrect to call the A copy constructor to initialize the parameter of
// f.

int fail;

struct A {
  A() { }
  A(const A&) { fail = 1; }
};
struct B : public A { };
struct X {
  operator B() { return B(); }
};
X x;

void f (const A&) { }

int main()
{
  f(x);
  return fail;
}
