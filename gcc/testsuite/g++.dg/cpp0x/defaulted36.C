// PR c++/53733
// { dg-do compile { target c++11 } }

template<typename T>
struct wrap
{
  wrap() = default;
  wrap(wrap&&) = default; // Line 5
  wrap(const wrap&) = default;

  T t;
};

struct S {
  S() = default;
  S(const S&){}
  S(S&&) = default;
};

typedef wrap<const S> W;

W get() { return W(); } // Line 19

int main() {}
