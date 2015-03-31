// PR c++/65390
// { dg-do compile }
// { dg-options "" }

template<typename T> struct shared_ptr { };

template<typename T, typename Arg>
shared_ptr<T> make_shared(Arg) { return shared_ptr<T>(); } // { dg-error "variably modified type|trying to instantiate" }

void f(int n){
  make_shared<int[n]>(1); // { dg-error "no matching function" }
}
