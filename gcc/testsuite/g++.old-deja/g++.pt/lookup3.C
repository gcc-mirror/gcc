// { dg-do run  }
template<class T>
class A {
public:
  void f() { }
};

class B : public A<int> {
public:
  void f();
};

int main()
{
  B b;
  B& b1 = b;
  b1.A<int>::f();
}
