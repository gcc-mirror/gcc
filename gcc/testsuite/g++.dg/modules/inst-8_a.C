// PR c++/122609
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules" }
// { dg-module-cmi std }

module;
#include <coroutine>
#include <tuple>
#include <typeinfo>
export module std;

// PR c++/101140
export using ::operator new;
export using ::operator delete;

export namespace std {
  using std::coroutine_handle;
  using std::coroutine_traits;
  using std::suspend_always;

  using std::get;
  using std::tuple;
  using std::tuple_size;
  using std::tuple_element;

  using std::type_info;
}
