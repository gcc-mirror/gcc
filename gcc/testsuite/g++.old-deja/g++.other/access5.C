// Build don't link:

class A
{
protected:
  int i;     
};

class B : private A
{
protected:
  A::i;
};

struct C : public B {
  friend int f(C *p);
};

int f(C *p) {
  return p->i;
}

