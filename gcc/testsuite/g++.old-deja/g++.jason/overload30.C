// { dg-do assemble  }
// PRMS Id: 6412

class Foo;

template <class F>
class Temp
{
  F  func_;
public:
  Temp (F f) : func_(f) {}
};

template <class T>
T* func1 (T* t) { return t; }

Temp<Foo*(*)(Foo*)> temp2(func1);
