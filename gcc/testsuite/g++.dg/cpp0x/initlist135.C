// PR c++/126472
// { dg-do run { target c++11 } }

#include <initializer_list>

namespace std {
struct _Optional_payload_base {
  struct _Storage {
    constexpr _Storage() : _M_empty() {}
#if __cpp_constexpr >= 201907L
    constexpr
#endif
    ~_Storage() {}
    int _M_empty;
  } _M_payload;
};
struct _Optional_payload : _Optional_payload_base {};
template <typename> struct optional {
  constexpr optional() {}
  template <typename _Up> optional(_Up) {}
  _Optional_payload _M_payload;
};
template <typename _T1, typename _T2> struct pair {
  _T1 first;
  _T2 second;
  constexpr pair(_T1 __x, _T2 __y) : first(__x), second(__y) { }
};
template <typename> struct vector {
  vector(int, int) {}
};
} // namespace std

using namespace std;
const initializer_list<pair<optional<vector<int>>, int>> data {
  pair<optional<vector<int>>, int>( optional<vector<int>>(vector<int>(1, 10)), 42), // non-constant
  pair<optional<vector<int>>, int>( optional<vector<int>>(), 100), // constant
};

int main() {
  if (data.begin()[0].second != 42 || data.begin()[1].second != 100)
    __builtin_abort();
}
