// Special g++ Options: -w

class A {
  int j;
};

class Space {
  int j;
};

typedef int (A::*pma)();

class B : public Space, virtual public A {
public:
  int i;
  int foo () {
    return i!=42;
  }
  B() {
    i = 42;
  }
};

int call_base (A* ap, pma pmf) {
  return (ap->*pmf)();
}

int main() {
  B b;
  return call_base ((A*)&b, (pma)&B::foo);
}
