// PR c++/90124
// { dg-do compile { target c++11 } }

class a {
public:
  int b;
};
class c : a {
  auto m_fn1() -> decltype(b);
};
