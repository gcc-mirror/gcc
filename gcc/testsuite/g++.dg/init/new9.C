// PR 12337

class A {};

template <typename T>
class X : public A {
public:
  X(T&);
};

class B {
public:
  bool foo(A*);
  template <typename T>
  bool foo(T& t) { return foo(new X<T>(t)); }
};

int main()
{
  B x, y;
  x.foo(y);
}
