// PR c++/108104
// { dg-do compile { target c++11 } }

struct A {
  void x();
  void y();
};

enum State { On };

template<State state, void (A::*)()>
struct B {
  static void f();
};

template<State state>
struct B<state, nullptr> {
  static void g();
};

template<State state>
struct B<state, &A::y> {
  static void h();
};

int main() {
  B<State::On, &A::x>::f();
  B<State::On, nullptr>::g();
  B<State::On, &A::y>::h();
}
