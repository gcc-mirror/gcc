// PR c++/18464

struct A
{
  A(int);
  operator void*() const;
};

template<int> void foo(const A& x) { 0 ? x : (x ? x : 0); }
