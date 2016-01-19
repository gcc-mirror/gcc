// PR c++/59759
// { dg-do compile { target c++11 } }

namespace std {
template <typename _Tp>
struct B {
  static constexpr _Tp value = 0;
};
typedef B<int> false_type;
struct C : false_type {};
template <typename>
struct is_integral : C {};
template <int, typename _Tp>
struct enable_if {
  typedef _Tp type;
};
}
enum class enabled;
constexpr enabled dummy{};
template <typename T, typename std::enable_if<std::is_integral<T>::value,
                                              enabled>::type = dummy>
class A;
template <typename T>
void f(A<T>*) {
  A<int>* map;
  f(map);
}
