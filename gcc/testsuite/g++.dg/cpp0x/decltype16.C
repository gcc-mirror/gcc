// PR c++/39070
// { dg-do compile { target c++11 } }

template<typename X> struct junk {
   template<typename Z> static Z y();
   template<typename Y> static int  test(...);
   template<typename Y> static char test(decltype(y<Y>())*);
   static int const value=sizeof(test<X>(0));
};
typedef char type[junk<int>::value==sizeof(char) ? 1 : -1];
