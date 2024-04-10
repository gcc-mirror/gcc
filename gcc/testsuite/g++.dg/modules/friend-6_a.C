// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi friend_6 }

module;
template <typename> struct Trans_NS___cxx11_basic_string {
  template <typename> friend class basic_stringbuf;
};
template struct Trans_NS___cxx11_basic_string<char>;
export module friend_6;
