// PR c++/66585
// { dg-do compile { target c++11 } }

class A {
  template <typename, typename> using _Requires = int;

public:
  template <typename _Functor, typename = _Requires<_Functor, void>>
  A(_Functor);
};
template <class T> class B {
  A f = [](T) {};
};

B<int> a;
