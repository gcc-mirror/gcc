typedef char uint8_t;
template <class T> class A {
public:
  A(T *);
};

enum TextDirection : uint8_t;
enum WritingMode : unsigned;
class B {
public:
  WritingMode m_fn1();
};
class C {
public:
  int &m_fn2();
};

class F {
public:
  F(C, A<const int>, B *, WritingMode, TextDirection);
};
class D { double d;};
class H : public D {};



template <class T> A<T>::A(T*) {}

template class A<H>;
template class A<int const>;

WritingMode __attribute__((noipa))
B::m_fn1()
{
  return (WritingMode) 0;
}

int gi;
int & __attribute__((noipa))
C::m_fn2 ()
{
  return gi;
}

__attribute__((noipa)) F::F(C, A<const int>, B *, WritingMode, TextDirection) {}
