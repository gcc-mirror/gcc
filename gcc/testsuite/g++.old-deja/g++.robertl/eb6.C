// Build don't link:
class A {
public:
  A() { t=0; }
  double t;
};
template <class T>
class B {
public:
  void f1() { new T; f2(); }
  void f2() { new T; }
};
template class B<A>;

