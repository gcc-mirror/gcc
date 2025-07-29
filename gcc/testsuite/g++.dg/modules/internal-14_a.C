// PR c++/120412
// { dg-additional-options "-fmodules -std=c++20 -Wtemplate-names-tu-local" }
// { dg-module-cmi m:part }

export module m:part;

export template <typename F>
auto fun1(F) {
  return true;
}

using Dodgy = decltype([]{});

export template <typename T>
auto fun2(T&&) {  // { dg-warning "TU-local" }
  return fun1(Dodgy{});
}
