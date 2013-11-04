// { dg-options "-std=gnu++11" }
template<typename... Types> 
  void f(Types... rest);

template<typename... Types> 
  void g(Types... rest) {
    f(&rest...); // ``&rest...'' is a pack expansion, ``&rest'' is its pattern
  }
