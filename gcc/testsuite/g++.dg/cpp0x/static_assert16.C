// PR c++/82239
// { dg-do compile { target c++11 } }

template<typename T>
struct C {
   static constexpr int x = 5;
   void f()
   {
      static_assert(0 < x, "");
      static_assert(0 < (x), "");
      static_assert(true || (0 < x), "");
      static_assert(true || (0 < (x)), "");
   }
};
