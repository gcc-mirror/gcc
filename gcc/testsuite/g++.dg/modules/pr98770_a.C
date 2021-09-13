// PR 98770 confused about duplicate template type aliases
// { dg-additional-options "-fmodules-ts -Wno-pedantic" }

module ;
# 6 __FILE__ 1
template<typename> using __void_t = void;
# 8 "" 2
export module Foo;

export using B = __void_t<int>;
