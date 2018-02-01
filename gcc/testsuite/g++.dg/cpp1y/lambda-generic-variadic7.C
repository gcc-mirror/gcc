// PR c++/82249
// { dg-do compile { target c++14 } }

template<class T, class U> T calc (T t, U u) { return t; }
template <class... Ts> void sink(Ts...);

template < typename ... Ds >
void f(Ds ...) {
  [](auto ... n){
    sink (calc(n, Ds{}) ...);
  }(Ds{} ...);
}


int main(){
  f();      // Wrong error
  f(0, 0);  // Wrong error
  f(0);     // ICE
}
