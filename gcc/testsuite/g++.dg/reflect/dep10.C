// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Partial substitution of a SPLICE_SCOPE.

template<typename>
void f()
{
   auto func = []<auto Mem>() static {
      return [: ^^[:Mem:] ::func :];
   };
}

void g() {
   f<int>();
}
