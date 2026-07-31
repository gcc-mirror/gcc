// PR c++/124489
// { dg-do compile { target c++11 } }

void
g ()
{
  constexpr decltype(nullptr) dm = nullptr; // { dg-message ".constexpr std::nullptr_t const dm. previously declared here" }
  constexpr decltype(nullptr) dm = nullptr; // { dg-error "redeclaration of .constexpr std::nullptr_t const dm." }
}
