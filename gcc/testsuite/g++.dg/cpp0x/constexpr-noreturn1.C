// { dg-do compile { target c++11 } }
// { dg-additional-options -Winvalid-constexpr }

// We were giving a wrong error about loading a volatile value instead of the
// proper error about calling a non-constexpr function.

[[noreturn]] void f();

constexpr int g()
{
  return f(), 42;	   // { dg-message "call to non-'constexpr' function" }
}
