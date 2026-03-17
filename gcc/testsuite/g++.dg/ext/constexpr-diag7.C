// PR c++/124404
// { dg-do compile { target c++26 } }

#include <string>

consteval {
  std::string msg = "my message";
  __builtin_constexpr_diag (2, "", msg);	// { dg-error "constexpr message: my message" }
}
