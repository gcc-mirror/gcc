// PR c++/119344
// { dg-do compile { target c++11 } }

struct S {
    void fn();
};
typedef void (S::*T)(void);
template <T Ptr>
struct s
{
  static const bool t = Ptr != T();
};

int t1 = s<&S::fn>::t;
