// PR c++/69095
// { dg-do compile { target c++11 } }

struct B1 {
  template <typename Ret, typename... Args, unsigned = sizeof(Args)> // { dg-error "parameter packs not expanded" }
  void insert(Ret);
};

struct B2 {
  template <typename Ret, typename... Args>
  void insert(Ret, unsigned = sizeof(Args)); // { dg-error "parameter packs not expanded" }
};

template <typename Ret, typename... Args, unsigned = sizeof(Args)> // { dg-error "parameter packs not expanded" }
void insert1(Ret);

template <typename Ret, typename... Args>
void insert2(Ret, unsigned = sizeof(Args)); // { dg-error "parameter packs not expanded" }
