// { dg-options "-std=gnu++98 -Wc++0x-compat" }
int static_assert; // { dg-warning "will become a keyword" }

void foo()
{
  static_assert = 5;
}
