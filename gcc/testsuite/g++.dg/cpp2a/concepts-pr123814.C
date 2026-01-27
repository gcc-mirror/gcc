// PR c++/123814
// { dg-do compile { target c++20 } }
// { dg-additional-options "-Wall --param ggc-min-expand=0 --param ggc-min-heapsize=0" }

namespace std {
  struct source_location {
    struct __impl {
      const char *_M_file_name;
      const char *_M_function_name;
      unsigned _M_line;
      unsigned _M_column;
    };
    static void current(const __impl* = __builtin_source_location());
  };
} // namespace std

template <typename> concept same_as = true;

template <typename... Us>
concept one_of = (same_as<Us> || ...);

template <one_of<> T> constexpr int buffer_size_for_int = 0;
template <> constexpr int buffer_size_for_int<int> = 1;

template <int> using Basic_Characters = int;

template <typename T>
Basic_Characters<buffer_size_for_int<T>> to_characters() {
  std::source_location::current();
} // { dg-warning "return statement" }

void go() {
  to_characters<int>();
}
