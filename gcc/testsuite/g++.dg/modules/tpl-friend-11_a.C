// PR c++/114275
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi M }

module;

template <typename... _Elements> struct T;

template <typename H> struct T<H> {
  template <typename...> friend struct T;
};

export module M;
export template <typename=void> void fun() { T<int> t; }
