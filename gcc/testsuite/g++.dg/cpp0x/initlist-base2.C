// PR c++/55922
// { dg-do run { target c++11 } }

bool called = false;

struct Base {
  Base() { if (called) throw 1; called = true; }
};

struct B1 : virtual Base {
  B1() { }
};

struct C : B1, virtual Base {
  C() : B1{}
  { }
};

int main() {
  C c;
}
