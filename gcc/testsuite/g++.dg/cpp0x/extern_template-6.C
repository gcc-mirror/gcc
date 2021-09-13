// PR c++/99066
// { dg-do compile { target c++11 } }

template <typename a> struct basic_string {
  static const int npos = 1;
};
template <typename a> const int basic_string<a>::npos;

struct e { template <bool> int f() const; };

template <bool> int e::f() const {
  return basic_string<char>::npos;
}

extern template class basic_string<char>;

// { dg-final { scan-assembler-not "_ZN12basic_stringIcE4nposE" } }
