// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

#include <exception>

constexpr std::exception_ptr
foo ()
{
  try
    {
      throw 42;
    }
  catch (...)
    {
      return std::current_exception ();
    }
}

constexpr bool
bar ()
{
  try
    {
      std::rethrow_exception (foo ());
    }
  catch (const int &a)
    {
       return a == 42;
    }
  return false;
}

static_assert (bar ());
constexpr std::exception_ptr a = foo ();				// { dg-error "is not a constant expression because it refers to exception object allocated with '__cxa_allocate_exception'" }
constexpr std::exception_ptr b = std::make_exception_ptr (42ULL);	// { dg-error "is not a constant expression because it refers to exception object allocated with '__cxa_allocate_exception'" }
