// PR c++/101078

struct A {
  static void f();
};

template<class>
struct B : private A {
  struct C {
    void g() { f(); }
    void g2() { B::f(); }
  };
};

int main() {
  B<int>::C().g();
}
