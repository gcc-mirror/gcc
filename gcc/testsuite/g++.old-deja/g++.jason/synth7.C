// { dg-do run  }
// Testcase to make sure that synthesized methods are found when needed.

struct B { ~B() { } };
struct A { B b; };

int main()
{
  A a, b (a), c = A();
  A& (A::*afp)(const A&) = &A::operator=;
  (a.*afp) (b);
}
