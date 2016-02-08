// PR c++/69283
// { dg-do compile { target c++14 } }

namespace Ape {
   struct Type {};

   template <typename T>
   auto f1(T const& v){
       return true;
   }

   template <typename T>
   auto f2(T const& v){
       return f2(v); // { dg-error "auto" }
   }
}

namespace Baboon {
   template <typename T>
   bool f3(T const& v){
       return f1(v);
   }

   template <typename T>
   bool f4(T const& v){
       f2(v);
   }
}

int main(){
   Ape::Type x;
   Baboon::f3(x);
   Baboon::f4(x);
}
