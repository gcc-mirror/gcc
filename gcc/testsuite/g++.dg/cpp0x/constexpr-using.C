// Core issue 898
// { dg-do compile { target c++11 } }

namespace N { const int i = 42; }
namespace M { const int j = 42; }

constexpr int g() {
  using namespace N;
  using M::j;
  static_assert (i == 42, "i == 42");
  return i + j;
}

template <class T>
constexpr int h() {
  using namespace N;
  using M::j;
  static_assert (i == 42, "i == 42");
  return i + j;
}

constexpr int i = g();
constexpr int i2 = h<int>();

static_assert (i == 84, "i == 84");
static_assert (i2 == 84, "i2 == 84");

