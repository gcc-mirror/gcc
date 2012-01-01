// { dg-do assemble  }

class A
{
protected:
  int i;     
};

class B : private A
{
protected:
  A::i; // { dg-warning "deprecated" }
};

struct C : public B {
  friend int f(C *p);
};

int f(C *p) {
  return p->i;
}

