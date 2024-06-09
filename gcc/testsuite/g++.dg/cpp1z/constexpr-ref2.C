// P2280R4 - Using unknown pointers and references in constant expressions
// PR c++/106650
// { dg-do compile { target c++17 } }

template <bool V>
struct Widget {
   struct Config {
      static constexpr bool value = V;
   } config;

   void f() {
       if constexpr (config.value) {
          // ...
       }
   }
};

void
g ()
{
  Widget<false> w;
  w.f();
}
