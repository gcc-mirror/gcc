// { dg-options "-std=gnu++98 -Wc++11-compat" }
int static_assert; // { dg-warning "is a keyword" }
int nullptr; // { dg-warning "is a keyword" }

void foo()
{
  static_assert = 5;
  nullptr = 5;
}
