// { dg-do assemble  }
// { dg-options "-fpermissive" }

class A
{
protected:
  void f1() {};
};

template <class T> class B : private A {
protected:
  using A::f1;
};

template <class T> class D : private B<T>
{
public:
  void f2() { f1(); }; // { dg-warning "" }
};

