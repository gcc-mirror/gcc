// Shows that problem of initializing one object's vtable pointer from
// another object's vtable pointer when doing a default copy of it
// and the vtable pointer involved is the main one.

// Correct answer is B::print.
// g++ prints D::print, which is wrong.  Cfront gets is right.

// prms-id: 2846

extern "C" int printf(const char *, ...);
extern "C" void exit(int);

class B {
public:
  virtual void print(void) const { printf("B::print\n"); }
};

class D : public B {
public:
  void print(void) const { printf("D::print\n"); exit(1); }
  B compute(void) const;
};

B D::compute(void) const
{
  B sub(*(B*)this);
  return sub;
}

int main () {
  D titi;
  titi.compute().print();
  return 0;
}
