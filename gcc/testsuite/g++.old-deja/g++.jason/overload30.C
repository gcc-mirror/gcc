// PRMS Id: 6412
// Build don't link:

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
