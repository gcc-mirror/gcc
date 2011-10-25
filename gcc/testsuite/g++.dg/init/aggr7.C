// PR c++/50866

struct A { A(); ~A(); };
struct B { B(const char *, const A& = A()); ~B(); };
struct C {
  B b1, b2;
};
void f()
{
     C c = {
          "a","b"
     };
}
