// { dg-do assemble  }
// GROUPS passed templates
template<class T> class X;
typedef X<int> IX;

template<class T>
class X {
public:
  T x;
};

struct A {
  IX c;
};
