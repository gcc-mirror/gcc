// PR c++/80119
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }

#include <type_traits>

template <bool b>
void failing_function(std::integral_constant<bool, b>)
{
   int i;
   if (b && (i = 4)) {
      ++i; // { dg-bogus "may be used uninitialized" }
   }
}

int main (void)
{
   failing_function(std::false_type());
}
