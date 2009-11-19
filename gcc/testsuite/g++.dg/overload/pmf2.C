// PR c++/561

class A { };

struct B : public A
{
  void foo ();
  void foo (int);
  template <class T>
  void bar (T);
  template <class T>
  void bar (T, T);
};

int main ()
{
  void (A::*f1)() = (void (A::*)()) &B::foo;
  void (A::*f2)(int) = (void (A::*)(int)) &B::bar;
  void (A::*f3)(int) = (void (A::*)(int)) &B::bar<int>;
}
