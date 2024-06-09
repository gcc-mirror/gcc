// Also test header modules
// PR c++/113405
// { dg-additional-options "-fmodule-header" }
// { dg-require-effective-target c++20 }
// { dg-module-cmi {} }

template <typename>
concept foo = false;

template <typename>
concept bar = true;

template <typename T>
struct corge {};

template <foo F>
struct corge<F> {};

template <bar B>
struct corge<B> {
  using alias = int;
};

template <typename T>
using corge_alias = corge<T>::alias;
