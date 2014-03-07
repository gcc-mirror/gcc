// PR c++/48735
// { dg-do compile { target c++11 } }

template<class T, 
 class = decltype(T{})
>
char f(int);

template<class>
char (&f(...))[2];

struct ND {
  // Make ND() non-aggregate.
  virtual void f();
  ND() = delete;
};

static_assert(sizeof(f<ND[1]>(0)) != 1, "Error");
