// PR c++/107267
// { dg-do compile { target c++11 } }
// { dg-additional-options -ffold-simple-inlines }

namespace std {
  template<typename _Tp> _Tp &&move(_Tp &&);
}

struct FindResult {
  FindResult();
  int result;
};

FindResult pop_ret = std::move(FindResult());
