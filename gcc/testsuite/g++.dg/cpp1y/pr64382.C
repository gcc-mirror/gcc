// PR c++/64382
// { dg-do compile { target c++14 } }

template<typename T>
struct my_queue
{
  void push(T)
  {
  }
  void ice()
  {
    auto L = [=](auto &&v) {
      push(v);
    };
    trav(L);
  }
  template<typename F>
  void trav(F &&f)
  {
    f(T());
  }
};
template struct my_queue<int>;
