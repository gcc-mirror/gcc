#pragma interface

template<class T>
struct C
{
  explicit C(const T& t) : a(t) { }
  virtual ~C() { }
  T a;
};


