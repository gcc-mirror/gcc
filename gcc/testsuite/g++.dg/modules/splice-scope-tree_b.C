// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodules -freflection" }

import exporting_splice_scope;

#include <meta>

int
main ()
{
  static_assert (std::meta::dealias (^^dependent_splice_type<int>) == ^^int);
  static_assert (std::meta::dealias (
      ^^somehow_more_complicated_dependent_splice_type<true, int, double>) == ^^int);
  static_assert (std::meta::dealias (
      ^^somehow_more_complicated_dependent_splice_type<false, int, double>) == ^^double);
}
