// Build don't link:
// Special g++ Options: -O
// prms-id: 8786

class B {
public:
  ~B();
};

class D : public B {
public:
  D(int);
};

int foo() {
  D t(0);

  bool h = 1;
  if (h) {
    D p(0);
    return 0;
  }
  return 0;
}
