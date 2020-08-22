// { dg-do compile { target c++20 } }

namespace std {
  struct source_location {
    struct __impl {
      const void *_M_file_name;
      const char *_M_function_name;
      int _M_line, _M_column;
    };
  };
}

auto x = __builtin_source_location ();	// { dg-error "'std::source_location::__impl::_M_file_name' does not have 'const char \\*' type" }
// { dg-message "evaluating '__builtin_source_location'" "" { target *-*-* } .-1 }
