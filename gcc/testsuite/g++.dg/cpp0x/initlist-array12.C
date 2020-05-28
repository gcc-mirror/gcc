// PR c++/95319
// { dg-do compile { target c++11 } }

typedef decltype(sizeof(char)) size_t;

namespace std {
template <class> class initializer_list {
  int *_M_array;
  size_t _M_len;
};
template <int _Nm> struct A { typedef int _Type[_Nm]; };
template <int _Nm> struct B { typename A<_Nm>::_Type _M_elems; };
class C {
public:
  void insert(int, B<3>);
  void insert(int, initializer_list<B<3>>);
};
} // namespace std
int a;
int
main() {
  using ArrayVector = std::C;
  auto b = ArrayVector();
  b.insert(a, {{2}});
  return 0;
}
