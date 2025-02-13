// PR c++/118856
// { dg-do compile { target c++11 } }

#include <initializer_list>

template <typename _Tp> struct vector {
  vector(std::initializer_list<_Tp>);
  template <typename T> vector(T, T);
  char * begin();
  char * end();
};

struct f{
  f(const char*);
};

void h() {
  for (auto &vec : vector<vector<f>>{{""}})
    ;
}
