// PR c++/66583
// { dg-do run { target c++11 } }
// { dg-options "" }

template <class T>
T&& move(T& t) { return static_cast<T&&>(t); }

struct A {
  A() { };
  A(const A&) { }
};

struct B {
  struct {
    int m_1 = 0;
    int m_2;
  };
  A dummy;
};

int main()
{
  B b;
  b.m_1 = 1;
  B c = move(b);
  if (c.m_1 != 1)
    __builtin_abort();
}
