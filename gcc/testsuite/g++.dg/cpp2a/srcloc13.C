// { dg-do compile { target c++2a } }

namespace std {
  struct source_location {
    struct __impl {
      const char *_M_file_name;
      const char *_M_function_name;
      float _M_line;
      int _M_column;
    };
  };
}

auto x = __builtin_source_location ();	// { dg-error "'std::source_location::__impl::_M_line' does not have integral type" }
// { dg-message "evaluating '__builtin_source_location'" "" { target *-*-* } .-1 }
