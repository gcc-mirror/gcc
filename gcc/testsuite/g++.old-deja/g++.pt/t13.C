// { dg-do assemble  }

template <class A> class B {
public:
  B();
  A a;
  int i;
};

void *f () {
  return new B<char *>;
}

struct foo { int i[10]; };
extern B<foo> *foop;

void f2 () {
  foop = new B<foo>;
}
