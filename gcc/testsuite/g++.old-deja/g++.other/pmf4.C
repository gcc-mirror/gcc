// Bug: g++ doesn't compensate for finding a virtual function in a
// non-primary vtable when generating PMFs.
// Submitted by Jason Merrill <jason@cygnus.com>

struct A {
  virtual ~A() {}
};

struct B {
  virtual void f () = 0;
};

struct C : public A, public B {
  void f ();
};

void (C::* B_f)() = &B::f;	// this works
void (C::* C_f)() = &C::f;	// this breaks

C* p;

void C::f ()
{
  p = this;
}

int main()
{
  C c;

  (c.*B_f)();
  if (p != &c)
    return 1;

  (c.*C_f)();
  if (p != &c)
    return 1;
}
