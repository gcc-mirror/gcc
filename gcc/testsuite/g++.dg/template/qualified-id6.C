// PR c++/54913

struct E
{
  static const int& e;
};

template<typename>
struct R
{
  R() { E::e; }
};

R<int> r;
