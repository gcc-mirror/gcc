// PR c++/47482

template<class>
struct K
{
  enum { A = sizeof"A", B = +A };
};
