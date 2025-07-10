// { dg-do compile { target c++20 } }

#if __cpp_constexpr_exceptions >= 202411L
namespace std {
  struct exception {
    constexpr exception () noexcept {}
    constexpr virtual ~exception () noexcept {}
    constexpr exception (const exception &) = default;
    constexpr exception &operator= (const exception &) = default;
    constexpr virtual const char *what () const noexcept { return "std::exception"; }
  };
  struct bad_alloc : public exception {
    constexpr virtual ~bad_alloc () noexcept {}
    constexpr virtual const char *what () const noexcept { return "std::bad_alloc"; }
  };
  struct bad_array_new_length : public bad_alloc {
    constexpr virtual ~bad_array_new_length () noexcept {}
    constexpr virtual const char *what () const noexcept { return "std::bad_array_new_length"; }
  };
}
#endif

constexpr int
foo (__SIZE_TYPE__ x, int y, int z)
{
  char (*a)[2] = new char[x][2];	// { dg-error "call to non-'constexpr' function 'void __cxa_throw_bad_array_new_length\\\(\\\)'" "" { target c++23_down } }
  delete[] a;				// { dg-message "declared here" "" { target c++23_down } .-1 }
  int *b = new int[y];			// { dg-error "call to non-'constexpr' function 'void __cxa_throw_bad_array_new_length\\\(\\\)'" "" { target c++23_down } }
  delete[] b;
  int *c = new int[z]{1, 2, 3};		// { dg-error "call to non-'constexpr' function 'void __cxa_throw_bad_array_new_length\\\(\\\)'" "" { target c++23_down } }
  delete[] c;
  return 0;
}

constexpr int a = foo (16, 2, 3);
constexpr int b = foo (-64, 2, 3);	// { dg-message "in 'constexpr' expansion of" "" { target c++23_down } }
					// { dg-error "uncaught exception" "" { target c++26 } .-1 }
constexpr int c = foo (16, -1, 3);	// { dg-message "in 'constexpr' expansion of" "" { target c++23_down } }
					// { dg-error "uncaught exception" "" { target c++26 } .-1 }
constexpr int d = foo (16, 2, 1);	// { dg-message "in 'constexpr' expansion of" "" { target c++23_down } }
					// { dg-error "uncaught exception" "" { target c++26 } .-1 }
