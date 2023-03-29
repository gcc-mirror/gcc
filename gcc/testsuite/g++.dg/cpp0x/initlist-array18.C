// PR c++/108773
// { dg-do compile { target c++11 } }

#include <initializer_list>

namespace std {
struct __new_allocator {};
struct allocator : __new_allocator {};
template <typename T>
struct basic_string {
  basic_string(const T *, allocator = allocator());
  ~basic_string();
};
using string = basic_string<char>;
template <typename T>
struct array {
  T _M_elems;
};
template <typename T>
struct list {
  list &operator=(initializer_list<T>);
};
}
struct RGWSyncTraceManager {
  std::list<std::array<std::string>> admin_commands;
  void hook_to_admin_command();
};
void RGWSyncTraceManager::hook_to_admin_command() {
  admin_commands = {{""}, {""}};
}
