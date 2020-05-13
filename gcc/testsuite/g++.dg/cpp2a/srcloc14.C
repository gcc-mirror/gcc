// { dg-do compile { target c++20 } }

namespace std {
  struct source_location {
    struct __impl {
      // Test that ordering doesn't matter
      long long _M_column;
      const char *_M_file_name;
      int _M_line;
      const char *_M_function_name;
    };
  };
}

auto x = __builtin_source_location ();
