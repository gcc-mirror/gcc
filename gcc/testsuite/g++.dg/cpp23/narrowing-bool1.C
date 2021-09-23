// P1401R5: Narrowing contextual conversions to bool
// { dg-do compile { target c++11 } }

void f() noexcept(sizeof(char[2])); // { dg-error "narrowing" } conversion of value 2 to type bool
void g() noexcept(sizeof(char)); // OK, conversion of value 1 to type bool is non-narrowing

#if __cpp_conditional_explicit
struct S {
  explicit(sizeof(char[2])) S(char); // { dg-error "narrowing" "" { target c++20 } }
  explicit(sizeof(char)) S(bool); // OK, conversion of value 1 to type bool is non-narrowing
};
#endif

static_assert(sizeof(int[2]), ""); // OK, narrowing allowed

#if __cpp_if_constexpr
int main()
{
  if constexpr (sizeof(int[2])) // OK, narrowing allowed
    {}
}
#endif
