// { dg-do assemble  }
// Testcase for explicit instantiation of templates.

template <class T>
class A {
  T t;
public:
  void f () { }
};

template class A<int>;

template <class T> T min (T a, T b) { return (a < b ? a : b); }

template int min (int, int);
