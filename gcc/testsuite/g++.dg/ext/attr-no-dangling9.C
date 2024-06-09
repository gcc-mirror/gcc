// PR c++/110358
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }

template<bool B>
struct bool_constant {
  static constexpr bool value = B;
  constexpr operator bool() const { return value; }
};

using true_type = bool_constant<true>;
using false_type = bool_constant<false>;

struct S {
  template<bool B>
  [[gnu::no_dangling(B)]] int &foo (const int &);
};

void
g ()
{
  [[maybe_unused]] const int &x0 = S().foo<false_type{}> (42);  // { dg-warning "dangling" }
  [[maybe_unused]] const int &x1 = S().foo<true_type{}> (42);  // { dg-bogus "dangling" }
}

