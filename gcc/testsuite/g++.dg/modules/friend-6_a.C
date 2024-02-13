// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
// { dg-module-cmi friend_6 }

module;
# 1 "" 1
template <typename> struct Trans_NS___cxx11_basic_string {
  template <typename> friend class basic_stringbuf;
};
template struct Trans_NS___cxx11_basic_string<char>;
# 6 "" 2
export module friend_6;
