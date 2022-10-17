// { dg-do compile { target c++20 } }

namespace std {
  struct source_location {
    struct __impl {
      const char *_M_file_name;
      const char *_M_function_name;
      unsigned int _M_line, _M_column;
    };
    const __impl *__ptr;
    constexpr source_location () : __ptr (nullptr) {}
    static consteval source_location
    current (const void *__p = __builtin_source_location ()) {
      source_location __ret;
      __ret.__ptr = static_cast <const __impl *> (__p);
      return __ret;
    }
    constexpr const char *file_name () const {
      return __ptr ? __ptr->_M_file_name : "";
    }
    constexpr const char *function_name () const {
      return __ptr ? __ptr->_M_function_name : "";
    }
    constexpr unsigned line () const {
      return __ptr ? __ptr->_M_line : 0;
    }
    constexpr unsigned column () const {
      return __ptr ? __ptr->_M_column : 0;
    }
  };
}

using namespace std;

auto a = source_location::current;		// { dg-error "taking address of an immediate function" }
constexpr auto b = &source_location::current;	// { dg-error "taking address of an immediate function" }

void
foo ()
{
  auto c = &source_location::current;		// { dg-error "taking address of an immediate function" }
  constexpr auto d = source_location::current;	// { dg-error "taking address of an immediate function" }
  static auto e = source_location::current;	// { dg-error "taking address of an immediate function" }
}
