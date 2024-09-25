// PR c++/116364
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi foo:part }

module;
template <typename> struct S {};
template <> struct S<int>
  { static constexpr bool value = true; };
export module foo:part;

export template <typename T>
  constexpr bool result = S<T>::value;
