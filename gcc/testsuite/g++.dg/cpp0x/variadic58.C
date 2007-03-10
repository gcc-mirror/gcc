// { dg-options "-std=gnu++0x" }
#include <typeinfo>

template<typename... Args>
void foo(Args...) { }

template<typename... Args>
void bar(Args... args) {
  foo(Args()...);
  foo(args = args...);
  foo(reinterpret_cast<void*>(&args)...);
  foo(const_cast<const Args>(args)...);
  foo(static_cast<void*>(&args)...);
  foo(dynamic_cast<void*>(&args)...);
  foo(typeid(Args)...);
}
