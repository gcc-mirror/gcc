// Bug: g++ complains about Z being a private base when trying to
// initialize B::foo.
// Build don't link:

struct Z {
  Z();
  Z(int);
};

struct A : private Z { };
struct B : public A
{
    Z foo;
    B();
    B(const B&);
};

B::B() : foo(1) { }		// gets bogus error
