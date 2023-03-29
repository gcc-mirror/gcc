// { dg-do run  }
// Shows that problem of initializing one object's secondary base from
// another object via a user defined copy constructor for that base,
// the pointer for the secondary vtable is not set after implicit
// copying of the outer class, but rather has the pointer to the main
// vtable for the secondary base left over from the user defined copy
// constructor for that base.

// Correct answer is B::beefy.
// g++ prints A::beefy, which is wrong.  Cfront gets it right.

// prms-id: 2846

extern "C" int printf(const char *, ...);
extern "C" void exit(int);

class B;

class A {
 public:

  A(void){}
  A(const A&){}

  virtual void print(void) const { }
  B compute(void) const;
};

class C {
public:
  C() { }
  C(C& o) { }		// with it, things are wrong, without it, they're ok
  virtual void beefy(void) const { printf("A::beefy\n"); exit(1); }
};

class B : private A, public C {
public:
  B(const A& x, int){}
  void beefy(void) const { printf("B::beefy\n"); }
};

B A::compute(void) const
{
  B sub(*this, 1);
  return static_cast<B&>(sub);
}

int main ()
{
  A titi;
  titi.compute().beefy();
  return 0;
}
