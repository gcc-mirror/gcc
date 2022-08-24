// { dg-do compile { target c++11 } }
// { dg-options "--param early-inlining-insns=14" }

struct Element;
template <int _Nm> struct __array_traits { typedef Element _Type[_Nm]; };
template <int _Nm> struct array {
  typename __array_traits<_Nm>::_Type _M_elems;
};
bool logLevel();
struct LogCapture {
  void stream();
};
struct Element {
  Element();
  long data_;
};
using ElementArray = array<6>;
struct ElementManager {
  ElementManager();
  ElementArray array_;
};
static ElementArray makeArray() {
  if (logLevel())
    LogCapture().stream();
  ElementArray foo;
  return foo;
}
ElementManager::ElementManager() : array_(makeArray()) {}
