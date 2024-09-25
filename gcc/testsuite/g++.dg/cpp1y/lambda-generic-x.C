// Explicit generic lambda test from N3690 5.1.2.5
// { dg-do compile { target c++14 } }
// { dg-options "-Wpedantic" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>

int main()
{
   auto glambda = [] <typename A, typename B> (A a, B&& b) { return a < b; };	// { dg-warning "lambda templates are only available with" "" { target c++17_down } }
   bool b = glambda(3, 3.14); // OK
   auto vglambda = [] <typename P> (P printer) {				// { dg-warning "lambda templates are only available with" "" { target c++17_down } }
     return [=] <typename... T> (T&& ... ts) { // OK: ts is a function parameter pack
       printer(std::forward<decltype(ts)>(ts)...);				// { dg-warning "lambda templates are only available with" "" { target c++17_down } .-1 }
       return [=]() {
         printer(ts ...);
       };
     };
   };
   auto p = vglambda( [] <typename A,						// { dg-warning "lambda templates are only available with" "" { target c++17_down } }
                          typename B,
                          typename C> (A v1, B v2, C v3)
     { std::cout << v1 << v2 << v3; } );
   auto q = p(1, 'a', 3.14); // OK: outputs 1a3.14
   q(); // OK: outputs 1a3.14
}

