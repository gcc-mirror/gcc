// Explicit generic lambda test from N3690 5.1.2.5
// { dg-options "-std=gnu++1y" }

#include <iostream>

int main()
{
   auto glambda = [] <typename A, typename B> (A a, B&& b) { return a < b; };
   bool b = glambda(3, 3.14); // OK
   auto vglambda = [] <typename P> (P printer) {
     return [=] <typename... T> (T&& ... ts) { // OK: ts is a function parameter pack
       printer(std::forward<decltype(ts)>(ts)...);
       return [=]() {
         printer(ts ...);
       };
     };
   };
   auto p = vglambda( [] <typename A,
                          typename B,
                          typename C> (A v1, B v2, C v3)
     { std::cout << v1 << v2 << v3; } );
   auto q = p(1, 'a', 3.14); // OK: outputs 1a3.14
   q(); // OK: outputs 1a3.14
}

