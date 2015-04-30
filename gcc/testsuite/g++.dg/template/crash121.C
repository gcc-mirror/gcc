// PR c++/59955

template< int xyz >
struct wovo {

   template< int n >
   void us(){}
   
   template< int n >
   struct us< n > {};  // { dg-error "template|conflicts" }
};
