// PR c++/117980
// { dg-do compile { target c++11 } }
// { dg-options "-O" }

struct _Safe_iterator  {
 _Safe_iterator();
  ~_Safe_iterator();
};
template <typename _Tp>
struct vector {
  vector(int) {}
  constexpr _Safe_iterator end() {
    return _Safe_iterator();
  }
};
template <typename It> struct sentinel {
  It it;
};
template <typename _Sent>
struct subrange {
  subrange(sentinel<_Safe_iterator>) {}
};
void test01() {
  vector<int> v{0};
  subrange<sentinel<_Safe_iterator>>{sentinel<_Safe_iterator>{v.end()}};
}
